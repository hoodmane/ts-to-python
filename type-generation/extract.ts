import {
  ClassDeclaration,
  ConstructSignatureDeclaration,
  EntityName,
  FunctionDeclaration,
  FunctionTypeNode,
  Identifier,
  InterfaceDeclaration,
  LiteralTypeNode,
  MethodSignature,
  Node,
  Project,
  PropertySignature,
  Signature,
  SignaturedDeclaration,
  SourceFile,
  SyntaxKind,
  TypeAliasDeclaration,
  TypeArgumentedNode,
  TypeElementTypes,
  TypeLiteralNode,
  TypeNode,
  TypeParameterDeclaration,
  TypeReferenceNode,
  UnionTypeNode,
  VariableDeclaration,
} from "ts-morph";
import {
  PyParam,
  PySig,
  PySigGroup,
  renderInnerSignature,
  renderProperty,
  renderPyClass,
  renderSignature,
  renderSignatureGroup,
  renderSimpleDeclaration,
  sanitizeReservedWords,
  uniqBy,
} from "./render.ts";
import { PyClass } from "./types.ts";
import {
  BUILTIN_NAMES,
  IMPORTS,
  TYPE_TEXT_MAP,
  getExtraBases,
  typeReferenceSubsitutions,
} from "./adjustments.ts";

import { groupBy, groupByGen, WrappedGen, split, popElt } from "./groupBy.ts";
import {
  ClassifiedIdentifier,
  GroupedBySyntaxKind,
  Needed,
  PyOther,
  PyTopLevel,
  Variance,
  reverseVariance,
} from "./types.ts";
import {
  classifyIdentifier,
  getExpressionTypeArgs,
  getNodeLocation,
  groupMembers,
} from "./astUtils.ts";
import { ParamIR, SigIR, TypeIR, sigToIR, typeToIR } from "./typeToIR.ts";

function assertUnreachable(_value: never): never {
  throw new Error("Statement should be unreachable");
}

function pyClass(name: string, supers: string[], body: string): PyClass {
  const superStems = supers
    .map((sup) => sup.split("[")[0])
    .filter((x) => x !== "Generic");
  return {
    kind: "class",
    name,
    superStems,
    supers,
    body,
  };
}

function pyOther(text: string): PyOther {
  return {
    kind: "other",
    text,
  };
}

function topologicalSortClasses(nameToCls: Map<string, PyClass>): PyClass[] {
  type AnotatedClass = PyClass & { sorted?: boolean; visited?: boolean };
  const result: PyClass[] = [];
  function visit(cls: AnotatedClass) {
    if (cls.sorted) {
      return;
    }
    if (cls.visited) {
      throw new Error("Cycle");
    }
    cls.visited = true;
    for (const name of cls.superStems) {
      const superClass = nameToCls.get(name);
      if (!superClass) {
        throw new Error(`Unknown super: ${cls.name} < ${name}`);
      }
      visit(superClass);
    }
    cls.sorted = true;
    result.push(cls);
  }
  for (const cls of nameToCls.values()) {
    visit(cls);
  }
  return result;
}

export class Converter {
  project: Project;
  convertedSet: Set<string>;
  neededSet: Set<Needed>;
  typeRefs: Set<Identifier>;
  typeParams: Set<string>;
  constructor() {
    this.project = new Project({
      tsConfigFilePath: "../type-generation-input-project/tsconfig.json",
      libFolderPath:
        "../type-generation-input-project/node_modules/typescript/lib",
    });
    this.convertedSet = new Set(BUILTIN_NAMES);
    this.neededSet = new Set();
    this.typeParams = new Set();
  }

  addNeededIdentifier(ident: Identifier): void {
    if (Node.isQualifiedName(ident)) {
      throw new Error("Qualified name! " + ident.getText());
    }
    this.neededSet.add({ type: "ident", ident });
  }

  addNeededInterface(ident: Identifier): void {
    this.neededSet.add({ type: "interface", ident });
  }

  getBaseNames(defs: (InterfaceDeclaration | ClassDeclaration)[]): string[] {
    let extends_ = defs.flatMap((def) => def.getExtends() || []);
    extends_ = uniqBy(extends_, (base) => base.getText());
    return extends_.flatMap((extend): string | [] => {
      let ident = extend.getExpression();
      if (!Node.isIdentifier(ident)) {
        return [];
      }
      this.addNeededInterface(ident);
      const name = extend.getExpression().getText();
      const typeArgNodes = getExpressionTypeArgs(ident, extend);
      // Unfortunately typescript doesn't expose getVariances on the type
      // checker, so we probably can't figure out what to put here.
      const pyArgs = typeArgNodes.map((node) =>
        this.typeToPython(node, false, Variance.none),
      );
      const args = pyArgs.length > 0 ? `[${pyArgs.join(", ")}]` : "";
      return name + "_iface" + args;
    });
  }

  emit(files: SourceFile[]): string[] {
    const varDecls = files.flatMap((file) => file.getVariableDeclarations());

    const topLevels: PyTopLevel[] = [];
    for (const varDecl of varDecls) {
      const name = sanitizeReservedWords(varDecl.getName());
      if (this.convertedSet.has(name)) {
        continue;
      }
      this.convertedSet.add(name);
      const result = this.convertVarDecl(varDecl);
      if (result) {
        topLevels.push(result);
      }
    }
    const funcDecls = files.flatMap((file) => file.getFunctions());
    const funcDeclsByName = groupBy(funcDecls, (decl) => decl.getName());
    for (const [name, decls] of Object.entries(funcDeclsByName)) {
      topLevels.push(...this.convertFuncDeclGroup(name, decls));
    }
    let next: Needed | undefined;
    while ((next = popElt(this.neededSet))) {
      if (next.type === "ident") {
        let res = this.convertNeededIdent(next.ident);
        if (res) {
          topLevels.push(res);
        }
        continue;
      }
      if (next.type === "interface") {
        const ident = next.ident;
        const name = ident.getText() + "_iface";
        if (this.convertedSet.has(name)) {
          continue;
        }
        this.convertedSet.add(name);

        const defs = ident
          .getDefinitionNodes()
          .filter(Node.isInterfaceDeclaration);
        if (defs.length) {
          const baseNames = this.getBaseNames(defs).filter(
            (base) => base !== name,
          );
          const typeParams = defs
            .flatMap((i) => i.getTypeParameters())
            .map((p) => p.getName());
          const res = this.convertInterface(
            name,
            baseNames,
            defs.flatMap((def) => def.getMembers()),
            [],
            typeParams,
          );
          topLevels.push(res);
          continue;
        }
        // console.warn(ident.getDefinitionNodes().map(n => n.getText()).join("\n\n"))
        console.warn("No interface declaration for " + name);
      }
    }
    const typevarDecls = Array.from(
      this.typeParams,
      (x) => `${x} = TypeVar("${x}")`,
    ).join("\n");
    const output = [IMPORTS, typevarDecls];
    const unsortedClasses = topLevels.filter(
      (x): x is PyClass => x.kind === "class",
    );
    const nameToCls = new Map(unsortedClasses.map((cls) => [cls.name, cls]));
    if (nameToCls.size < unsortedClasses.length) {
      throw new Error("Duplicate");
    }
    // We need to ensure that the supers are topologically sorted so that we respect the MRO.
    const classes = topologicalSortClasses(nameToCls);
    const classNameToIndex = new Map(
      classes.map((cls, idx) => [cls.name, idx]),
    );
    for (const cls of classes) {
      const extraBases = getExtraBases(cls.name);
      if (extraBases) {
        cls.supers.push(...extraBases);
        cls.concrete = true;
      } else {
        for (const sName of cls.superStems) {
          const s = nameToCls.get(sName);
          if (s.concrete) {
            cls.concrete = true;
            break;
          }
        }
      }
      if (cls.name.endsWith("_iface") && !cls.concrete) {
        cls.supers.push("Protocol");
      }
      if (!cls.name.endsWith("_iface")) {
        cls.supers.push("_JsObject");
      }
    }
    for (const topLevel of topLevels) {
      switch (topLevel.kind) {
        case "class":
          topLevel.supers.sort((a, b) => {
            a = a.split("[")[0];
            b = b.split("[")[0];
            return classNameToIndex.get(b) - classNameToIndex.get(a);
          });
          output.push(renderPyClass(topLevel));
          break;

        case "other":
          const { text } = topLevel;
          output.push(text);
          break;
      }
    }
    return output;
  }

  getInterfaceTypeParams(ident: Identifier): string[] {
    return Array.from(
      new Set(
        ident
          .getDefinitionNodes()
          .filter(Node.isInterfaceDeclaration)
          .flatMap((def) => def.getTypeParameters())
          .map((param) => param.getName()),
      ),
    );
  }

  convertNeededIdent(ident: Identifier): PyTopLevel | undefined {
    const name = ident.getText();
    if (this.convertedSet.has(name)) {
      return undefined;
    }
    this.convertedSet.add(name);
    if (Node.isQualifiedName(ident)) {
      throw new Error("Qualified name!");
    }
    const classified = classifyIdentifier(ident);
    switch (classified.kind) {
      case "interfaces":
        const ifaces = classified.ifaces;
        const baseNames = this.getBaseNames(ifaces);
        const typeParams = ifaces
          .flatMap((i) => i.getTypeParameters())
          .map((p) => p.getName());

        return this.convertInterface(
          name,
          baseNames,
          ifaces.flatMap((def) => def.getMembers()),
          [],
          typeParams,
        );
      case "class":
        if (classified.ifaces.length > 0) {
          throw new Error("Unhandled");
        }
        return this.convertClass(classified.decl);
      case "typeAlias":
        const renderedType = this.typeToPython(
          classified.decl.getTypeNode()!,
          false,
          Variance.covar,
        );
        return pyOther(`${name} = ${renderedType}`);
      case "varDecl":
        console.warn("Skipping varDecl", ident.getText());
    }
    return undefined;
  }

  renderSimpleDecl(name: string, typeNode: TypeNode): string {
    const renderedType = this.typeToPython(typeNode, false, Variance.covar);
    return renderSimpleDeclaration(name, renderedType);
  }

  convertFuncDeclGroup(name: string, decls: FunctionDeclaration[]): PyOther[] {
    return renderSignatureGroup(
      this.overloadGroupToPython(
        name,
        decls.map((x) => x.getSignature()),
      ),
      false,
    ).map(pyOther);
  }

  convertVarDecl(varDecl: VariableDeclaration): PyTopLevel | undefined {
    const name = sanitizeReservedWords(varDecl.getName());
    const typeNode = varDecl.getTypeNode()!;
    if (!typeNode) {
      return undefined;
    }
    if (Node.isTypeLiteral(typeNode)) {
      // declare var X : {}
      //
      // If it looks like declare var X : { prototype: Blah, new(paramspec): ret}
      // then X is the constructor for a class
      //
      // Otherwise it's a global namespace object?
      try {
        return this.convertMembersDeclaration(name, typeNode, [], []);
      } catch (e) {
        console.warn(varDecl.getText());
        console.warn(getNodeLocation(varDecl));
        throw e;
      }
    }
    if (Node.isTypeReference(typeNode)) {
      // This also could be a constructor like `declare X: XConstructor` where
      // XConstructor has a prototype and 'new' signatures. Or not...
      return this.convertVarDeclOfReferenceType(name, typeNode);
    }
    if (Node.isIntersectionTypeNode(typeNode)) {
      if (varDecl.getTypeNode().getText() === "Window & typeof globalThis") {
        return pyOther(renderSimpleDeclaration(name, "Window"));
      }
      console.warn("intersection varDecl:", varDecl.getText());
      return undefined;
    }
    return pyOther(this.renderSimpleDecl(name, typeNode));
  }

  convertVarDeclOfReferenceType(
    name: string,
    typeNode: TypeReferenceNode,
  ): PyTopLevel {
    // declare var A : B;

    // Cases:
    //   A is a constructor ==> inline B
    //   o/w don't...
    const ident = typeNode.getTypeName() as Identifier;
    if (!ident.getDefinitionNodes) {
      console.warn(ident.getText());
      return undefined;
    }

    const classified = classifyIdentifier(ident);
    if (classified.kind === "varDecl" && name !== classified.name) {
      // We have to check that name !== typeName or else we can pick up the decl
      // we're currently processing.
      return pyOther(this.renderSimpleDecl(name, typeNode));
    }
    if (classified.kind === "varDecl" || classified.kind === "interfaces") {
      const { ifaces } = classified;
      const typeParams = ifaces
        .flatMap((i) => i.getTypeParameters())
        .map((p) => p.getName());
      return this.convertMembersDeclaration(
        name,
        {
          getMembers: () => ifaces.flatMap((iface) => iface.getMembers()),
        },
        [],
        typeParams,
      );
    }
    if (classified.kind === "class") {
      return pyOther(renderSimpleDeclaration(name, classified.decl.getName()));
    }
    if (classified.kind === "typeAlias") {
      return pyOther(
        this.renderSimpleDecl(name, classified.decl.getTypeNode()),
      );
    }
    assertUnreachable(classified);
  }

  convertMembersDeclaration(
    name: string,
    type: { getMembers: TypeLiteralNode["getMembers"] },
    bases = [],
    typeParams: string[],
  ): PyClass {
    const [prototypes, staticMembers] = split(
      type.getMembers(),
      (m): m is PropertySignature =>
        m.isKind(SyntaxKind.PropertySignature) && m.getName() === "prototype",
    );
    let members: TypeElementTypes[] = [];
    for (const proto of prototypes) {
      const typeNode = proto.getTypeNode();
      if (!Node.isTypeReference(typeNode)) {
        console.warn(
          "Excepted prototype type to be TypeReference",
          proto.getText(),
        );
        continue;
      }
      const ident = typeNode.getTypeName() as Identifier;
      this.addNeededInterface(ident);
      const name = ident.getText() + "_iface";
      const typeParams = this.getInterfaceTypeParams(ident);
      const arg = typeParams.length ? `[${typeParams.join(",")}]` : "";
      const base = name + arg;
      bases.push(base);
    }
    return this.convertInterface(
      name,
      bases,
      members,
      staticMembers,
      typeParams,
    );
  }

  convertSignatures(
    sigs: readonly Signature[],
    variance: Variance,
    topLevelName?: string,
  ): string {
    return this.convertIRSignatures(sigs.map(sigToIR), variance, topLevelName);
  }

  convertIRSignatures(
    irSigs: readonly SigIR[],
    variance: Variance,
    topLevelName?: string,
  ): string {
    const pySigs = irSigs.map((sig) => this.sigIRToPython(sig, variance));
    if (!topLevelName) {
      const converted = pySigs.map(renderInnerSignature);
      return converted.join(" | ");
    }
    const converted = pySigs.map((sig) => renderSignature(topLevelName, sig));
    if (converted.length === 1) {
      return converted[0];
    }
    return converted.map((x) => "@overload\n" + x).join("\n\n");
  }

  convertSignature(
    sig: Signature,
    variance: Variance,
    topLevelName?: string,
  ): string {
    const sigIR = sigToIR(sig);
    const pySig = this.sigIRToPython(sigIR, variance);
    if (topLevelName) {
      return renderSignature(topLevelName, pySig);
    }
    return renderInnerSignature(pySig);
  }

  getInterfaceDeclToDestructure(
    sig: Signature,
  ): InterfaceDeclaration | undefined {
    const decl = sig.getDeclaration() as SignaturedDeclaration;
    const defs = decl
      .getParameters()
      .at(-1)
      ?.getTypeNode()
      .asKind(SyntaxKind.TypeReference)
      ?.getTypeName()
      ?.asKind(SyntaxKind.Identifier)
      ?.getDefinitionNodes();
    if (defs?.length !== 1) {
      return undefined;
    }
    return defs[0].asKind(SyntaxKind.InterfaceDeclaration);
  }

  sigToPythonDestructure(
    sig: Signature,
    variance: Variance,
    decorators: string[] = [],
  ): PySig[] {
    const sigIR = sigToIR(sig);
    const pySig = this.sigIRToPython(sigIR, variance, decorators);

    const toDestructure = this.getInterfaceDeclToDestructure(sig);
    if (!toDestructure) {
      return [pySig];
    }
    const pySigDestructured = structuredClone(pySig);
    pySigDestructured.params.pop();
    const paramVariance = reverseVariance(variance);
    const kwargs: PyParam[] = [];
    for (const prop of toDestructure.getProperties()) {
      const name = prop.getName();
      const optional = !!prop.getQuestionTokenNode();
      const pyType = this.typeToPython(
        prop.getTypeNode()!,
        optional,
        paramVariance,
      );
      kwargs.push({ name, pyType, optional });
    }
    pySigDestructured.kwparams = kwargs;
    return [pySig, pySigDestructured];
  }

  sigIRToPython(
    sig: SigIR,
    variance: Variance,
    decorators: string[] = [],
  ): PySig {
    const convertParam = ({ name, optional, type }: ParamIR): PyParam => {
      const pyType = this.typeIRToPython(type, optional, paramVariance);
      return { name, optional, pyType };
    };
    const {
      params: origParams,
      spreadParam: origSpreadParam,
      kwparams: origKwparams,
      returns: origReturns,
    } = sig;
    const paramVariance = reverseVariance(variance);
    const params = origParams.map(convertParam);
    const kwparams = origKwparams?.map(convertParam);
    const spreadParam = origSpreadParam
      ? convertParam(origSpreadParam)
      : undefined;
    const returns = this.typeIRToPython(origReturns, false, variance);
    return { params, spreadParam, kwparams, returns, decorators };
  }

  sigToPython(
    sig: Signature,
    variance: Variance,
    decorators: string[] = [],
  ): PySig {
    const sigIR = sigToIR(sig);
    return this.sigIRToPython(sigIR, variance, decorators);
  }

  convertClass(decl: ClassDeclaration): PyClass {
    // MethodDeclaration | PropertyDeclaration | GetAccessorDeclaration | SetAccessorDeclaration | ConstructorDeclaration | ClassStaticBlockDeclaration;
    const name = decl.getName();
    const supers = this.getBaseNames([decl]);
    const staticMembers = decl.getStaticMembers();
    const members = decl.getInstanceMembers();
    const [methodDecls, rest] = split(members, Node.isMethodDeclaration);
    const methodGroups = groupBy(methodDecls, (d) => d.getName());
    const methodEntries = Object.entries(methodGroups).flatMap(([name, sigs]) =>
      renderSignatureGroup(
        this.overloadGroupToPython(
          name,
          sigs.map((decl) => decl.getSignature()),
        ),
        true,
      ),
    );
    const [staticMethodDecls, staticRest] = split(
      staticMembers,
      Node.isMethodDeclaration,
    );
    const staticMethodGroups = groupBy(staticMethodDecls, (d) => d.getName());
    const staticMethodEntries = Object.entries(staticMethodGroups).flatMap(
      ([name, sigs]) =>
        renderSignatureGroup(
          this.overloadGroupToPython(
            name,
            sigs.map((decl) => decl.getSignature()),
            Variance.covar["classmethod"],
          ),
          true,
        ),
    );
    for (const member of rest) {
      if (Node.isPropertyDeclaration(member)) {
        methodEntries.push(
          this.renderSimpleDecl(member.getName(), member.getTypeNode()),
        );
        continue;
      }
      throw new Error(`Unhandled member kind ${member.getKindName()}`);
    }
    for (const member of staticRest) {
      throw new Error(`Unhandled static member kind ${member.getKindName()}`);
    }
    const entries = methodEntries.concat(staticMethodEntries);
    return pyClass(name, supers, entries.join("\n"));
  }

  convertInterface(
    name: string,
    supers: string[],
    members: TypeElementTypes[],
    staticMembers: TypeElementTypes[],
    typeParams: string[],
  ): PyClass {
    const { methods, properties } = groupMembers(members);
    const {
      methods: staticMethods,
      properties: staticProperties,
      constructors,
    } = groupMembers(staticMembers);
    const propMap = new Map(properties.map((prop) => [prop.getName(), prop]));
    for (const key of Object.keys(staticMethods)) {
      delete methods[key];
    }
    if (typeParams.length > 0) {
      const typeParamsList = Array.from(new Set(typeParams)).join(",");
      supers.push(`Generic[${typeParamsList}]`);
    }
    const extraEntries: string[] = [];
    if ("[Symbol.iterator]" in methods) {
      const x = methods["[Symbol.iterator]"];
      delete methods["[Symbol.iterator]"];
      const typeNode = x[0]
        .getDeclaration()
        .getReturnTypeNode()
        .asKindOrThrow(SyntaxKind.TypeReference);
      if (typeNode.getTypeName().getText() !== "IterableIterator") {
        throw new Error("Surprise!");
      }
      const typeArg = typeNode.getTypeArguments()[0];
      const pyType = this.typeToPython(typeArg, false, Variance.covar);
      const returns = `PyIterator[${pyType}]`;
      const entries = renderSignatureGroup(
        { name: "__iter__", sigs: [{ params: [], returns }] },
        true,
      );
      extraEntries.push(...entries);
    }
    if (
      name !== "Function_iface" &&
      (propMap.get("size")?.getTypeNode().getText() === "number" ||
        propMap.get("length")?.getTypeNode().getText() === "number")
    ) {
      const entries = renderSignatureGroup(
        { name: "__len__", sigs: [{ params: [], returns: "int" }] },
        true,
      );
      extraEntries.push(...entries);
    }
    const redirectMethod = (origName: string, newName: string): void => {
      if (!(origName in methods)) {
        return;
      }
      const entries = renderSignatureGroup(
        this.overloadGroupToPython(newName, methods[origName]),
        true,
      );
      extraEntries.push(...entries);
    };
    redirectMethod("has", "__contains__");
    redirectMethod("includes", "__contains__");
    redirectMethod("get", "__getitem__");
    redirectMethod("set", "__setitem__");
    redirectMethod("delete", "__delitem__");
    const overloadGroups = Object.entries(methods).map(([name, sigs]) =>
      this.overloadGroupToPython(name, sigs),
    );
    if (constructors) {
      staticMethods["new"] = constructors.map((decl) => decl.getSignature());
    }

    const staticOverloadGroups = Object.entries(staticMethods).map(
      ([name, sigs]) => this.overloadGroupToPython(name, sigs, ["classmethod"]),
    );
    const renderedProps: [string, string][] = properties.map((prop) => [
      prop.getName(),
      this.convertPropertySignature(prop),
    ]);
    const renderedStaticProps: [string, string][] = staticProperties.map(
      (prop) => [prop.getName(), this.convertPropertySignature(prop, true)],
    );
    renderedProps.push(...renderedStaticProps);
    const props = uniqBy(renderedProps, ([name]) => name).map(
      ([_, prop]) => prop,
    );
    const pyMethods = overloadGroups
      .concat(staticOverloadGroups)
      .flatMap((gp) => renderSignatureGroup(gp, true));
    const entries = props.concat(pyMethods, extraEntries);
    return pyClass(name, supers, entries.join("\n"));
  }

  convertPropertySignature(
    member: PropertySignature,
    isStatic: boolean = false,
  ): string {
    const memberName = member.getName();
    const isOptional = member.hasQuestionToken();
    const pytype = this.typeToPython(
      member.getTypeNode()!,
      isOptional,
      Variance.covar,
      memberName,
    );
    let readOnly = member.isReadonly();
    return renderProperty(memberName, pytype, readOnly, isStatic);
  }

  overloadGroupToPython(
    name: string,
    signatures: Signature[],
    decorators: string[] = [],
  ): PySigGroup {
    const sigs = signatures.flatMap((sig) =>
      this.sigToPythonDestructure(sig, Variance.covar, decorators),
    );
    return { name, sigs };
  }

  typeToPython(
    typeNode: TypeNode,
    isOptional: boolean,
    variance: Variance,
    topLevelName?: string,
  ): string {
    if (isOptional) {
      topLevelName = undefined;
    }
    const ir = typeToIR(typeNode);
    return this.typeIRToPython(ir, isOptional, variance, topLevelName);
  }

  typeIRToPython(
    ir: TypeIR,
    isOptional: boolean,
    variance: Variance,
    topLevelName?: string,
  ): string {
    let result = this.typeIRToPythonInner(ir, variance, topLevelName);
    if (!isOptional) {
      return result;
    }
    if (
      (ir.kind === "simple" && ir.text === "None") ||
      (ir.kind === "union" &&
        ir.types.some((x) => x.kind === "simple" && x.text === "None"))
    ) {
      return result;
    }
    result += " | None";
    return result;
  }

  typeIRToPythonInner(
    ir: TypeIR,
    variance: Variance,
    topLevelName?: string,
  ): string {
    if (ir.kind === "simple") {
      return ir.text;
    }
    if (ir.kind === "union") {
      return ir.types
        .map((ty) => this.typeIRToPython(ty, false, variance))
        .join(" | ");
    }
    if (ir.kind === "intersection") {
      console.warn("No conversion for intersection at " + ir.location);
      return "Any";
    }
    if (ir.kind === "paren") {
      const inner = this.typeIRToPython(ir.type, false, variance);
      return `(${inner})`;
    }
    // if (type.getCallSignatures().length > 0) {
    //   return this.convertSignatures(
    //     type.getCallSignatures(),
    //     variance,
    //     topLevelName,
    //   );
    // }
    if (ir.kind === "array") {
      const eltType = this.typeIRToPython(ir.type, false, variance);
      if (variance === Variance.contra) {
        return `PyMutableSequence[${eltType}]`;
      }
      return `JsArray[${eltType}]`;
    }
    if (ir.kind == "tuple") {
      let elts = ir.types
        .map((elt) => this.typeIRToPython(elt, false, variance))
        .join(", ");
      if (elts === "") {
        elts = "()";
      }
      return `tuple[${elts}]`;
    }
    if (ir.kind === "operator") {
      // Ignore type operators
      return this.typeIRToPython(ir.type, false, variance);
    }
    if (ir.kind === "callable") {
      return this.convertIRSignatures(ir.signatures, variance, topLevelName);
    }
    if (ir.kind === "other") {
      return "Any";
    }
    if (ir.kind === "parameterReference") {
      this.typeParams.add(ir.name);
      return ir.name;
    }
    if (ir.kind === "reference") {
      const { ident, typeArgs } = ir;
      let name = ir.ident.getText();
      const res = typeReferenceSubsitutions(this, name, typeArgs, variance);
      if (res) {
        return res;
      }
      if (!this.convertedSet.has(name)) {
        if (Node.isQualifiedName(ident)) {
          return "Any";
        }
        let kind: ClassifiedIdentifier["kind"];
        ({ name, kind } = classifyIdentifier(ident));
        if (kind === "interfaces") {
          this.addNeededInterface(ident);
        } else {
          this.addNeededIdentifier(ident);
        }
      }
      const args = typeArgs.map((ty) =>
        this.typeIRToPython(ty, false, Variance.none),
      );
      let fmtArgs = "";
      if (args.length) {
        fmtArgs = `[${args.join(", ")}]`;
      }
      return `${name}${fmtArgs}`;
    }
    assertUnreachable(ir);
  }
}

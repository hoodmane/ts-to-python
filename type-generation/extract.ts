import {
  ClassDeclaration,
  FunctionDeclaration,
  Identifier,
  InterfaceDeclaration,
  Node,
  Project,
  PropertySignature,
  Signature,
  SignaturedDeclaration,
  SourceFile,
  SyntaxKind,
  TypeElementTypes,
  TypeLiteralNode,
  TypeNode,
  TypeReferenceNode,
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
  getExtraBases,
  typeReferenceSubsitutions,
} from "./adjustments.ts";

import { groupBy, split, popElt } from "./groupBy.ts";
import {
  ClassifiedIdentifier,
  Needed,
  PyOther,
  PyTopLevel,
  Variance,
  reverseVariance,
} from "./types.ts";
import {
  assertUnreachable,
  classifyIdentifier,
  getExpressionTypeArgs,
  getNodeLocation,
} from "./astUtils.ts";
import {
  BaseIR,
  InterfaceIR,
  ParamIR,
  PropertyIR,
  SigGroupIR,
  SigIR,
  TopLevelIR,
  TypeIR,
  callableToIR,
  declsToBases,
  funcDeclsToIR,
  interfaceToIR,
  membersDeclarationToIR,
  propertySignatureToIR,
  sigToIR,
  sigToIRDestructure,
  typeToIR,
  varDeclToIR,
} from "./astToIR.ts";

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
    const bases = declsToBases(defs);
    return bases.map((base) => this.renderBase(base));
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
          const baseNames = declsToBases(defs).filter(
            (base) => base.name !== name,
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
        const baseNames = declsToBases(ifaces);
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
        const ir = typeToIR(classified.decl.getTypeNode()!);
        const renderedType = this.renderTypeIR(ir, false, Variance.covar);
        return pyOther(`${name} = ${renderedType}`);
      case "varDecl":
        console.warn("Skipping varDecl", ident.getText());
    }
    return undefined;
  }

  convertFuncDeclGroup(name: string, decls: FunctionDeclaration[]): PyOther[] {
    const sigsIR = funcDeclsToIR(name, decls);
    return this.renderSignatureGroup(sigsIR, false).map(pyOther);
  }

  convertVarDecl(astVarDecl: VariableDeclaration): PyTopLevel | undefined {
    const irVarDecl = varDeclToIR(astVarDecl);
    return this.renderTopLevelIR(irVarDecl);
  }

  convertClass(decl: ClassDeclaration): PyClass {
    throw new Error("TODO not implemented");
  }

  convertInterface(
    name: string,
    supers: BaseIR[],
    members: TypeElementTypes[],
    staticMembers: TypeElementTypes[],
    typeParams: string[],
  ): PyClass {
    const irInterface = interfaceToIR(
      name,
      supers,
      members,
      staticMembers,
      typeParams,
    );
    return this.renderInterface(irInterface);
  }

  renderTopLevelIR(toplevel: TopLevelIR): PyTopLevel {
    if (toplevel.kind === "declaration") {
      const { name, type } = toplevel;
      const typeStr = this.renderTypeIR(type, false, Variance.covar);
      return pyOther(renderSimpleDeclaration(name, typeStr));
    }
    if (toplevel.kind === "interface") {
      return this.renderInterface(toplevel);
    }
    assertUnreachable(toplevel);
  }

  renderIRSignatures(
    irSigs: readonly SigIR[],
    variance: Variance,
    topLevelName?: string,
  ): string {
    const pySigs = irSigs.map((sig) => this.renderSig(sig, variance));
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

  renderSig(
    sig: SigIR,
    variance: Variance,
    decorators: string[] = [],
    isStatic: boolean = false,
  ): PySig {
    const renderParam = ({ name, optional, type }: ParamIR): PyParam => {
      const pyType = this.renderTypeIR(type, optional, paramVariance);
      return { name, optional, pyType };
    };
    const {
      params: origParams,
      spreadParam: origSpreadParam,
      kwparams: origKwparams,
      returns: origReturns,
    } = sig;
    const paramVariance = reverseVariance(variance);
    const params = origParams.map(renderParam);
    const kwparams = origKwparams?.map(renderParam);
    const spreadParam = origSpreadParam
      ? renderParam(origSpreadParam)
      : undefined;
    const returns = this.renderTypeIR(origReturns, false, variance);
    if (isStatic) {
      decorators.push("classmethod");
    }
    return { params, spreadParam, kwparams, returns, decorators };
  }

  renderSignatureGroup(
    { name, sigs, isStatic }: SigGroupIR,
    isMethod: boolean,
  ): string[] {
    const pySigs = sigs.map((sig) =>
      this.renderSig(sig, Variance.covar, [], isStatic),
    );
    return renderSignatureGroup({ name, sigs: pySigs }, isMethod);
  }

  renderBase({ name, ident, typeParams }: BaseIR): string {
    if (ident) {
      this.addNeededInterface(ident as Identifier);
    }
    if (typeParams.length > 0) {
      const joined = typeParams
        .map((t) => this.renderTypeIR(t, false, Variance.covar))
        .join(", ");
      name += `[${joined}]`;
    }
    return name;
  }

  renderInterface({
    name,
    properties,
    methods,
    typeParams,
    bases,
  }: InterfaceIR): PyClass {
    const entries = ([] as string[]).concat(
      properties.map((prop) => this.renderProperty(prop)),
      methods.flatMap((gp) => this.renderSignatureGroup(gp, true)),
    );
    const newSupers = bases.map((b) => this.renderBase(b));
    if (typeParams.length > 0) {
      const joined = typeParams.join(", ");
      newSupers.push(`Generic[${joined}]`);
    }
    return pyClass(name, newSupers, entries.join("\n"));
  }

  renderProperty(property: PropertyIR): string {
    const { isOptional, name, type, isReadonly, isStatic } = property;
    const pyType = this.renderTypeIR(type, isOptional, Variance.covar, name);
    return renderProperty(name, pyType, isReadonly, isStatic);
  }

  renderTypeIR(
    ir: TypeIR,
    isOptional: boolean,
    variance: Variance,
    topLevelName?: string,
  ): string {
    if (isOptional) {
      topLevelName = undefined;
    }
    let result = this.renderTypeIRInner(ir, variance, topLevelName);
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

  renderTypeIRInner(
    ir: TypeIR,
    variance: Variance,
    topLevelName?: string,
  ): string {
    if (ir.kind === "simple") {
      return ir.text;
    }
    if (ir.kind === "union") {
      return ir.types
        .map((ty) => this.renderTypeIR(ty, false, variance))
        .join(" | ");
    }
    if (ir.kind === "intersection") {
      console.warn("No conversion for intersection at " + ir.location);
      return "Any";
    }
    if (ir.kind === "paren") {
      const inner = this.renderTypeIR(ir.type, false, variance);
      return `(${inner})`;
    }
    if (ir.kind === "array") {
      const eltType = this.renderTypeIR(ir.type, false, variance);
      if (variance === Variance.contra) {
        return `PyMutableSequence[${eltType}]`;
      }
      return `JsArray[${eltType}]`;
    }
    if (ir.kind == "tuple") {
      let elts = ir.types
        .map((elt) => this.renderTypeIR(elt, false, variance))
        .join(", ");
      if (elts === "") {
        elts = "()";
      }
      return `tuple[${elts}]`;
    }
    if (ir.kind === "operator") {
      // Ignore type operators
      return this.renderTypeIR(ir.type, false, variance);
    }
    if (ir.kind === "callable") {
      return this.renderIRSignatures(ir.signatures, variance, topLevelName);
    }
    if (ir.kind === "other") {
      return "Any";
    }
    if (ir.kind === "parameterReference") {
      this.typeParams.add(ir.name);
      return ir.name;
    }
    if (ir.kind === "reference") {
      let { identName: name, ident, typeArgs } = ir;
      const res = typeReferenceSubsitutions(this, name, typeArgs, variance);
      if (res) {
        return res;
      }
      // negative identIndex means this is a manually inserted type so we don't
      // have to handle it.
      if (ident && !this.convertedSet.has(name)) {
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
        this.renderTypeIR(ty, false, Variance.none),
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

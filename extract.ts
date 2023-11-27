import {
  Project,
  SymbolFlags,
  Node,
  VariableDeclaration,
  InterfaceDeclaration,
  SyntaxKind,
  Type,
  TypeLiteralNode,
  PropertySignature,
  TypeElementTypes,
  MethodSignature,
  ConstructSignatureDeclaration,
  TypeAliasDeclaration,
  ModuleDeclaration,
  VariableStatement,
  ModuleDeclarationKind,
  SignaturedDeclaration,
  FunctionTypeNode,
  Signature,
  TypeNode,
  TypeFlags,
  UnionTypeNode,
  Identifier,
  ImportSpecifier,
  LiteralTypeNode,
  SourceFile,
  ClassDeclaration,
  TypeReferenceNode,
} from "ts-morph";
import * as ts from "typescript";
import {
  renderSignatureGroup,
  renderSignature,
  renderInnerSignature,
  renderProperty,
  renderPyClass,
  PySig,
  PyParam,
  uniqBy,
  PySigGroup,
  sanitizeReservedWords,
} from "./render.ts";

import { groupBy, groupByGen, WrappedGen, split, popElt } from "./groupBy.ts";

function getNodeLocation(node: Node) {
  const sf = node.getSourceFile();
  const { line, column } = sf.getLineAndColumnAtPos(node.getStart());
  return `${sf.getFilePath()}:${line}:${column}`;
}

type SyntaxKindMap = {
  [SyntaxKind.VariableStatement]: VariableStatement;
  [SyntaxKind.VariableDeclaration]: VariableDeclaration;
  [SyntaxKind.PropertySignature]: PropertySignature;
  [SyntaxKind.MethodSignature]: MethodSignature;
  [SyntaxKind.ConstructSignature]: ConstructSignatureDeclaration;
  [SyntaxKind.InterfaceDeclaration]: InterfaceDeclaration;
  [SyntaxKind.TypeAliasDeclaration]: TypeAliasDeclaration;
  [SyntaxKind.ModuleDeclaration]: ModuleDeclaration;
};

type GroupedBySyntaxKind = { [K in keyof SyntaxKindMap]?: SyntaxKindMap[K][] };

function groupBySyntaxKind(list: Iterable<Node>): GroupedBySyntaxKind {
  const gen = groupBySyntaxKindGen();
  for (const x of list) {
    gen.next(x);
  }
  return gen.done();
}

function groupBySyntaxKindGen(): WrappedGen<Node, GroupedBySyntaxKind> {
  return groupByGen<Node, any>((node) => {
    return node.getKind();
  });
}

function groupMembers(members: TypeElementTypes[]): {
  methods: Record<string, Signature[]>;
  properties: PropertySignature[];
  constructors: ConstructSignatureDeclaration[];
} {
  const grouped = groupBySyntaxKind(members);
  const allProperties = grouped[SyntaxKind.PropertySignature] || [];
  const { functions = [], properties = [] } = groupBy(allProperties, (prop) =>
    prop.getTypeNode()?.isKind(SyntaxKind.FunctionType)
      ? "functions"
      : "properties",
  );
  const methodSigs = grouped[SyntaxKind.MethodSignature] || [];
  const empty: [string, FunctionTypeNode | MethodSignature][] = [];
  const methodOrFuncProps = empty.concat(
    methodSigs.map((meth) => [meth.getName(), meth]),
    functions.map((func) => [
      func.getName(),
      func.getTypeNode() as FunctionTypeNode,
    ]),
  );
  const methodNamePairs = groupBy(methodOrFuncProps, ([name, prop]) => name);
  const methods = Object.fromEntries(
    Object.entries(methodNamePairs).map(([name, v]) => [
      name,
      v.map(([_, prop]) => prop.getSignature()),
    ]),
  );
  const constructors = grouped[SyntaxKind.ConstructSignature] || [];
  return { methods, properties, constructors };
}

function filterSignatures(name: string, signatures: Signature[]): Signature[] {
  // if (["assign", "fromEntries"].includes(name)) {
  //   signatures = signatures.filter(
  //     (sig) => !sig.typeParameters || sig.typeParameters.length === 0,
  //   );
  // }
  // if (name === "getOwnPropertyDescriptors") {
  //   const anyNode = ts.factory.createKeywordTypeNode(ts.SyntaxKind.AnyKeyword);
  //   // @ts-ignore
  //   signatures[0].type = anyNode;
  // }
  // if (name === "bind") {
  //   const anyNode = ts.factory.createKeywordTypeNode(ts.SyntaxKind.AnyKeyword);
  //   const thisArgId = ts.factory.createIdentifier("thisArg");
  //   const thisArg = ts.factory.createParameterDeclaration(
  //     undefined,
  //     undefined,
  //     thisArgId,
  //     undefined,
  //     anyNode,
  //   );
  //   const decl = ts.factory.createMethodSignature(
  //     undefined,
  //     "bind",
  //     undefined,
  //     undefined,
  //     [thisArg],
  //     anyNode,
  //   );
  //   return [decl];
  // }
  return signatures;
}

const IMPORTS = `
from collections.abc import Callable
from asyncio import Future
from typing import overload, Any, Literal, Self

from pyodide.ffi import JsIterable as Iterable, JsIterator as Iterator
IterableIterator = Iterator
`.trim();

export class Converter {
  project: Project;
  convertedSet: Set<string>;
  neededSet: Set<Identifier>;
  constructor() {
    this.project = new Project({
      tsConfigFilePath: "./input_example/tsconfig.json",
      libFolderPath: "./input_example/node_modules/typescript/lib",
    });
    this.convertedSet = new Set(["Iterable", "Iterator", "IterableIterator"]);
    this.neededSet = new Set();
  }

  main(allFiles: boolean) {
    console.warn({ allFiles });
    let files: SourceFile[];
    this.project.addSourceFilesAtPaths("input_example/a.ts");
    if (allFiles) {
      files = this.project.resolveSourceFileDependencies();
      // console.warn(files.map(file => file.getFilePath()));
    } else {
      files = [this.project.getSourceFile("input_example/a.ts")!];
    }
    console.log(this.emit(files).join("\n\n"));
  }

  getBaseNames(
    baseDeclarations: (
      | InterfaceDeclaration
      | TypeAliasDeclaration
      | ClassDeclaration
    )[],
  ) {
    // Hack: if we were "extend" a type alias then for some reason we seem to
    // get the value of the TypeAlias, not the TypeAliasDeclaration. Then
    // getNameNode won't exist...
    baseDeclarations = baseDeclarations.filter((b) => b.getNameNode);
    baseDeclarations = uniqBy(baseDeclarations, (base) => base.getName());
    baseDeclarations.forEach((b) => this.neededSet.add(b.getNameNode()));
    return baseDeclarations.map((base) => base.getName());
  }

  emit(files: SourceFile[]): string[] {
    const varDecls = files.flatMap((file) => file.getVariableDeclarations());

    const output: string[] = [IMPORTS];
    for (const varDecl of varDecls) {
      const name = sanitizeReservedWords(varDecl.getName());
      if (this.convertedSet.has(name)) {
        continue;
      }
      this.convertedSet.add(name);
      const result = this.convertVarDecl(varDecl);
      if (result) {
        output.push(result);
      }
    }
    let ident: Identifier | undefined;
    while ((ident = popElt(this.neededSet))) {
      const name = ident.getText();
      if (this.convertedSet.has(name)) {
        continue;
      }
      this.convertedSet.add(name);
      if (!ident.getDefinitionNodes) {
        console.warn("Skipped", name);
        continue;
      }
      const defs = ident.getDefinitionNodes();
      if (defs.every(Node.isInterfaceDeclaration)) {
        const baseNames = this.getBaseNames(
          defs.flatMap((def) => def.getBaseDeclarations()),
        );
        output.push(
          this.convertInterface(
            name,
            baseNames,
            defs.flatMap((def) => def.getMembers()),
          ),
        );
        continue;
      }
      if (defs.length === 1) {
        const def = defs[0];
        if (Node.isTypeAliasDeclaration(def)) {
          const renderedType = this.typeToPython(def.getTypeNode()!, false);
          output.push(`${name} = ${renderedType}`);
          continue;
        }
      }

      console.warn("Skipping", ident.getText());
    }

    return output;
  }

  renderSimpleDecl(name: string, typeNode: TypeNode) {
    const renderedType = this.typeToPython(typeNode, false);
    return `${name}: ${renderedType}`;
  }

  convertVarDecl(varDecl: VariableDeclaration): string | undefined {
    const name = sanitizeReservedWords(varDecl.getName());
    const typeNode = varDecl.getTypeNode()!;
    if (!typeNode) {
      return undefined;
    }
    if (Node.isTypeLiteral(typeNode)) {
      // declare X : {}
      //
      // If it looks like declare var X : { prototype: Blah, new(paramspec): ret}
      // then X is the constructor for a class
      //
      // Otherwise it's a global namespace object?
      try {
        return this.convertMembersDeclaration(name, typeNode);
      } catch (e) {
        console.warn(varDecl.getText());
        console.warn(getNodeLocation(varDecl));
        throw e;
      }
    }
    if (Node.isTypeReference(typeNode)) {
      // This also could be a constructor like `declare X: XConstructor` where
      // XConstructor has a prototype and 'new' signatures. Or not...
      this.convertVarDeclOfReferenceType(name, typeNode);
    }
    const intersectionRef = typeNode.asKind(SyntaxKind.IntersectionType);
    if (intersectionRef) {
      console.warn("intersection varDecl:", varDecl.getText());
      return undefined;
    }
    return this.renderSimpleDecl(name, typeNode);
  }

  convertVarDeclOfReferenceType(name: string, typeNode: TypeReferenceNode) {
    const ident = typeNode.getTypeName() as Identifier;
    if (!ident.getDefinitionNodes) {
      console.warn(ident.getText());
      return undefined;
    }

    const typeName = ident.getText();
    const definitions = ident.getDefinitionNodes();
    // We'll do casework in how the type was defined.
    if (name !== typeName && definitions.some(Node.isVariableDeclaration)) {
      // Assertion: definitions include one VariableDeclaration and zero or
      // more InterfaceDeclarations.
      //
      // We have to be careful to ensure name !== typeName or else we can
      // pick up the decl we're currently processing.
      //
      // The type has a variable declaration so we'll handle it in this same
      // loop.
      return this.renderSimpleDecl(name, typeNode);
    }
    const typeAlias = ident
      .getDefinitionNodes()
      .filter(Node.isTypeAliasDeclaration)[0];
    if (typeAlias) {
      // If it's a type alias, look up the value of the
      // aliased type and render that
      return this.renderSimpleDecl(name, typeNode);
    }
    // Otherwise hopefully every definition node is an interface (possibly
    // excluding the current varDecl)
    const ifaces = ident
      .getDefinitionNodes()
      .filter(Node.isInterfaceDeclaration);
    const protoIface = ifaces.filter(
      (iface) => !!iface.getProperty("prototype"),
    )[0];
    if (protoIface) {
      const bases = this.getBaseNames(protoIface.getBaseDeclarations());
      return this.convertMembersDeclaration(name, protoIface, bases);
    }
    return this.convertMembersDeclaration(name, {
      getMembers: () => ifaces.flatMap((iface) => iface.getMembers()),
    });
  }

  convertMembersDeclaration(
    name: string,
    type: { getMembers: TypeLiteralNode["getMembers"] },
    bases = [],
  ): string {
    const { prototype, staticMembers } = groupBy(type.getMembers(), (m) => {
      if (
        m.isKind(SyntaxKind.PropertySignature) &&
        m.getName() === "prototype"
      ) {
        return "prototype";
      } else {
        return "staticMembers";
      }
    });
    let members: TypeElementTypes[] = [];
    if (prototype) {
      if (prototype.length > 1) {
        throw new Error("Didn't expect to see multiple prototype fields...");
      }
      const proto = prototype[0] as PropertySignature;
      const typeNode = proto.getTypeNode();
      if (typeNode?.isKind(SyntaxKind.TypeReference)) {
        const ident = typeNode.getTypeName() as Identifier;
        if (ident.getText() === name) {
          const decls = ident
            .getDefinitionNodes()
            .filter(Node.isInterfaceDeclaration);
          members = decls.flatMap((node) => node.getMembers());
          bases.push(
            ...this.getBaseNames(
              decls.flatMap((decl) => decl.getBaseDeclarations()),
            ),
          );
        }
      } else {
        console.warn(
          "Excepted prototype type to be TypeReference",
          proto.getText(),
        );
      }
    }
    return this.convertInterface(name, bases, members, staticMembers);
  }

  convertSignatures(sigs: readonly Signature[], topLevelName?: string): string {
    const converted = sigs.map((sig) =>
      this.convertSignature(sig, topLevelName),
    );
    if (!topLevelName) {
      return converted.join(" | ");
    }

    if (converted.length === 1) {
      return converted[0];
    }
    return converted.map((x) => "@overload\n" + x).join("\n\n");
  }

  convertSignature(sig: Signature, topLevelName?: string): string {
    const pySig = this.sigToPython(sig);
    if (topLevelName) {
      return renderSignature(topLevelName, pySig);
    }
    return renderInnerSignature(pySig);
  }

  sigToPython(sig: Signature, decorators: string[] = []): PySig {
    const decl = sig.getDeclaration() as SignaturedDeclaration;
    try {
      const params = decl.getParameters().map((param) => {
        const spread = !!param.getDotDotDotToken();
        const optional = !!param.hasQuestionToken();
        const pyType = this.typeToPython(param.getTypeNode()!, optional);
        return { name: param.getName(), pyType, optional, spread };
      });
      const retNode = decl.getReturnTypeNode()!;
      const returns = this.typeToPython(retNode, false);
      return { params, returns, decorators };
    } catch (e) {
      console.warn("failed to convert", sig.getDeclaration().getText());
      throw e;
    }
  }

  convertInterface(
    name: string,
    supers: string[],
    members: TypeElementTypes[],
    staticMembers: TypeElementTypes[] = [],
  ) {
    const { methods, properties } = groupMembers(members);
    const {
      methods: staticMethods,
      properties: staticProperties,
      constructors,
    } = groupMembers(staticMembers);
    for (const key of Object.keys(staticMethods)) {
      delete methods[key];
    }

    const overloadGroups = Object.entries(methods)
      .filter(([name]) => !name.includes("["))
      .map(([name, sigs]) => this.overloadGroupToPython(name, sigs));
    if (constructors) {
      staticMethods["new"] = constructors.map((decl) => decl.getSignature());
    }
    const staticOverloadGroups = Object.entries(staticMethods)
      .filter(([name]) => !name.includes("["))
      .map(([name, sigs]) =>
        this.overloadGroupToPython(name, sigs, ["classmethod"]),
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
      .flatMap((gp) => renderSignatureGroup(gp));
    const entries = props.concat(pyMethods);
    return renderPyClass(name, supers, entries.join("\n"));
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
    signatures = filterSignatures(name, signatures);
    const sigs = signatures.map((sig) => this.sigToPython(sig, decorators));
    return { name, sigs };
  }

  typeToPython(
    typeNode: TypeNode,
    isOptional: boolean,
    topLevelName?: string,
  ): string {
    if (isOptional) {
      topLevelName = undefined;
    }
    let inner = this.typeToPythonInner(typeNode, isOptional, topLevelName);
    if (isOptional && !Node.isUnionTypeNode(typeNode)) {
      inner += " | None";
    }
    return inner;
  }

  unionTypeNodeToPython(typeNode: UnionTypeNode, isOptional: boolean): string {
    const unionTypes = typeNode.getTypeNodes() as TypeNode[];
    const [literals, rest] = split<TypeNode, LiteralTypeNode>(
      unionTypes,
      Node.isLiteralTypeNode,
    );
    const types = rest.map((ty) => this.typeToPython(ty, false));
    const lits = literals
      .map((lit) => lit.getText())
      .filter((txt) => {
        if (txt === "null") {
          isOptional = true;
          return false;
        }
        return true;
      })
      .map((txt) => {
        if (txt === "true") {
          return "True";
        }
        if (txt === "false") {
          return "False";
        }
        return txt;
      });
    if (lits.length > 0) {
      types.push(`Literal[${lits.join(", ")}]`);
    }
    if (isOptional) {
      types.push("None");
    }
    return types.join(" | ");
  }

  typeToPythonInner(
    typeNode: TypeNode,
    isOptional: boolean,
    topLevelName?: string,
  ): string {
    const type = typeNode.getType();
    const typeText = typeNode.getText();
    if (typeText === "number") {
      return "int | float";
    }
    // @ts-ignore
    if (typeText === "bigint") {
      return "int";
    }
    if (typeText === "boolean") {
      return "bool";
    }
    if (typeText === "string") {
      return "str";
    }
    if (["void", "undefined", "null"].includes(typeText)) {
      return "None";
    }
    // @ts-ignore
    if (typeText === "symbol") {
      return "Symbol";
    }
    if (["any", "unknown"].includes(typeText)) {
      return "Any";
    }
    if (typeText === "object") {
      // "object"...
      return "Any";
    }
    if (Node.isThisTypeNode(typeNode)) {
      return "Self";
    }
    if (Node.isUnionTypeNode(typeNode)) {
      return this.unionTypeNodeToPython(typeNode, isOptional);
    }
    if (Node.isParenthesizedTypeNode(typeNode)) {
      const ty = this.typeToPython(typeNode.getTypeNode(), false);
      return `(${ty})`;
    }
    if (Node.isIntersectionTypeNode(typeNode)) {
      const filteredTypes = typeNode
        .getTypeNodes()
        .filter(
          (type) =>
            !(
              Node.isThisTypeNode(type) ||
              type.getText().startsWith("ThisType<")
            ),
        );
      if (filteredTypes.length === 1) {
        return this.typeToPython(filteredTypes[0], false);
      }
      const typeString = type.getText();
      if (typeString === "Window & typeof globalThis") {
        return "Any";
      }
      if (typeString === "ArrayBufferLike & { BYTES_PER_ELEMENT?: never; }") {
        return "ArrayBuffer";
      }
    }
    if (Node.isTypeLiteral(typeNode)) {
      // return checker.typeToString(type);
      return "Any";
    }
    if (Node.isLiteralTypeNode(typeNode)) {
      let text = typeNode.getText();
      if (text === "null") {
        return "None";
      }
      if (text === "true") {
        text = "True";
      }
      if (text === "false") {
        text = "False";
      }
      return `Literal[${text}]`;
    }
    if (type.getCallSignatures().length > 0) {
      return this.convertSignatures(type.getCallSignatures(), topLevelName);
    }
    if (Node.isArrayTypeNode(typeNode)) {
      const eltType = this.typeToPython(typeNode.getElementTypeNode(), false);
      return `list[${eltType}]`;
    }
    if (Node.isTupleTypeNode(typeNode)) {
      let elts = typeNode
        .getElements()
        .map((elt) => this.typeToPython(elt, false))
        .join(", ");
      if (elts === "") {
        elts = "()";
      }
      return `tuple[${elts}]`;
    }
    if (Node.isTypeReference(typeNode)) {
      if (typeNode.getType().isTypeParameter()) {
        return "Any";
      }
      const args = typeNode
        .getTypeArguments()
        .map((ty) => this.typeToPython(ty, false))
        .join(", ");
      let fmtArgs = "";
      if (args) {
        fmtArgs = `[${args}]`;
      }
      const ident = typeNode.getTypeName() as Identifier;
      let name = ident.getText();
      if (name.startsWith("Intl")) {
        return "Any";
      }
      if (name === "Promise") {
        name = "Future";
      } else if (name === "Function") {
        name = "Callable";
      } else if (
        !typeNode.getType().isTypeParameter() &&
        !this.convertedSet.has(name)
      ) {
        this.neededSet.add(ident);
      }
      return `${name}${fmtArgs}`;
    }
    if (Node.isTypeOperatorTypeNode(typeNode)) {
      // Just ignore readonly
      const operator = typeNode.getOperator();
      typeNode.getOperator();
      if (
        [SyntaxKind.ReadonlyKeyword, SyntaxKind.UniqueKeyword].includes(
          operator,
        )
      ) {
        return this.typeToPython(typeNode.getTypeNode(), false);
      }
      return "Any";
      throw new Error("Unknown type operator " + operator);
    }
    if (Node.isTemplateLiteralTypeNode(typeNode)) {
      return "str";
    }
    if (Node.isConstructorTypeNode(typeNode)) {
      return "Any";
    }
    if (Node.isMappedTypeNode(typeNode)) {
      return "Any";
    }
    if (Node.isIndexedAccessTypeNode(typeNode)) {
      return "Any";
    }
    if (Node.isTypeQuery(typeNode)) {
      return "Any";
    }
    if (Node.isConditionalTypeNode(typeNode)) {
      return "Any";
    }
    if (Node.isTypePredicate(typeNode)) {
      return "bool";
    }
    console.warn(typeNode.getKindName());
    console.warn(
      `No known conversion for '${type.getText()}'\n ${getNodeLocation(
        typeNode,
      )}`,
    );
    return "Any";
  }
}

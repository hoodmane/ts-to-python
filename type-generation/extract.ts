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
  TypeParameter,
  TypeParameterDeclaration,
  ExpressionWithTypeArguments,
  TypeArgumentedNode,
  FunctionDeclaration,
  EntityName,
} from "ts-morph";
import * as ts from "typescript";
import {
  renderSignatureGroup,
  renderSignature,
  renderInnerSignature,
  renderProperty,
  renderPyClass,
  renderSimpleDeclaration,
  PySig,
  PyParam,
  uniqBy,
  PySigGroup,
  sanitizeReservedWords,
} from "./render.ts";

import { groupBy, groupByGen, WrappedGen, split, popElt } from "./groupBy.ts";

Error.stackTraceLimit = Infinity;

function assertUnreachable(_value: never): never {
  throw new Error("Statement should be unreachable");
}

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

const IMPORTS = `
from collections.abc import Callable
from asyncio import Future
from typing import overload, Any, Literal, Self, TypeVar, Generic, ClassVar, Never, Protocol

from pyodide.ffi import JsProxy, JsIterable as Iterable, JsIterator as Iterator, JsArray, JsMutableMap as Map, JsMap as ReadonlyMap
from pyodide.webloop import PyodideFuture as PromiseLike
Promise = PromiseLike
ConcatArray = JsArray
Array = JsArray
ArrayLike = JsArray
Dispatcher = Any
URL_ = URL

class Record(JsProxy, Generic[S, T]):
  pass

class IterableIterator(Iterator[T], Iterable[T], Generic[T]):
  pass
`.trim();
const BUILTIN_NAMES = [
  "Iterable",
  "Iterator",
  "IterableIterator",
  "ArrayLike",
  "Array",
  "ConcatArray",
  "PromiseLike",
  "Promise",
  "Map",
  "ReadonlyMap",
  "Readonly",
  "Record",
  "Dispatcher",
  "_URL_iface",
  "_URLSearchParams_iface",
  "__Blob_iface",
  "__Event_iface",
  "__EventTarget_iface",
];

type Needed =
  | { type: "ident"; ident: Identifier }
  | { type: "interface"; ident: Identifier };

function getExpressionTypeArgs(
  ident: EntityName,
  expression: TypeArgumentedNode & Node,
): TypeNode[] {
  if (Node.isQualifiedName(ident)) {
    ident = ident.getRight();
  }
  const typeArgNodes = expression.getTypeArguments();
  const numTypeArgs = expression.getType().getTypeArguments().length;
  if (typeArgNodes.length < numTypeArgs) {
    const seenNames: string[] = [];
    const paramDecls: TypeParameterDeclaration[] = [];
    const defs = ident
      .getDefinitionNodes()
      .filter(
        (node): node is InterfaceDeclaration | TypeAliasDeclaration =>
          Node.isInterfaceDeclaration(node) ||
          Node.isTypeAliasDeclaration(node),
      );
    for (const def of defs) {
      const params = def.getTypeParameters();
      for (const param of params) {
        const paramName = param.getName();
        if (!seenNames.includes(paramName)) {
          seenNames.push(paramName);
          paramDecls.push(param);
        }
      }
    }
    const missingDecls = paramDecls.slice(-(numTypeArgs - typeArgNodes.length));
    for (const decl of missingDecls) {
      typeArgNodes.push(decl.getDefaultOrThrow());
    }
  }
  return typeArgNodes;
}

type InterfacesIdentifier = {
  kind: "interfaces";
  name: string;
  ifaces: InterfaceDeclaration[];
};
type TypeAliasIdentifier = {
  kind: "typeAlias";
  name: string;
  decl: TypeAliasDeclaration;
};
type ClassIdentifier = {
  kind: "class";
  name: string;
  decl: ClassDeclaration;
  ifaces: InterfaceDeclaration[];
};
type VarDeclIdentifier = {
  kind: "varDecl";
  name: string;
  decl: VariableDeclaration;
  ifaces: InterfaceDeclaration[];
};

type ClassifiedIdentifier =
  | InterfacesIdentifier
  | TypeAliasIdentifier
  | ClassIdentifier
  | VarDeclIdentifier;

function classifyIdentifier(ident: Identifier): ClassifiedIdentifier {
  let name = ident.getText();
  const defs = ident.getDefinitionNodes();
  const [ifaces, rest] = split(defs, Node.isInterfaceDeclaration);
  if (rest.length === 0) {
    name += "_iface";
    return {
      kind: "interfaces",
      name,
      ifaces,
    };
  }
  if (rest.length > 1) {
    throw new Error("Oops!");
  }
  const decl = rest[0];
  if (Node.isClassDeclaration(decl)) {
    return {
      kind: "class",
      name,
      decl,
      ifaces,
    };
  }
  if (Node.isVariableDeclaration(decl)) {
    return {
      kind: "varDecl",
      name,
      decl,
      ifaces,
    };
  }

  if (Node.isTypeAliasDeclaration(decl)) {
    if (ifaces.length > 0) {
      throw new Error("Both interfaces and type aliases with same name...");
    }
    return {
      kind: "typeAlias",
      name,
      decl,
    };
  }
  throw new Error("Unrecognized ident!");
}

export class Converter {
  project: Project;
  convertedSet: Set<string>;
  neededSet: Set<Needed>;
  typeRefs: Set<Identifier>;
  typeParams: Set<string>;
  constructor() {
    this.project = new Project({
      tsConfigFilePath: "../../input_example/tsconfig.json",
      libFolderPath: "../../input_example/node_modules/typescript/lib",
    });
    this.convertedSet = new Set(BUILTIN_NAMES);
    this.neededSet = new Set();
    this.typeParams = new Set();
  }

  addNeededIdentifier(ident: Identifier) {
    if (Node.isQualifiedName(ident)) {
      throw new Error("Qualified name! " + ident.getText());
    }
    this.neededSet.add({ type: "ident", ident });
  }

  addNeededInterface(ident: Identifier) {
    this.neededSet.add({ type: "interface", ident });
  }

  getBaseNames(defs: (InterfaceDeclaration | ClassDeclaration)[]) {
    let extends_ = defs.flatMap((def) => def.getExtends() || []);
    extends_ = uniqBy(extends_, (base) => base.getText());
    return extends_.flatMap((extend) => {
      let ident = extend.getExpression();
      if (!Node.isIdentifier(ident)) {
        return [];
      }
      this.addNeededInterface(ident);
      const name = extend.getExpression().getText();
      const typeArgNodes = getExpressionTypeArgs(ident, extend);
      const pyArgs = typeArgNodes.map((node) => this.typeToPython(node, false));
      const args = pyArgs.length > 0 ? `[${pyArgs.join(", ")}]` : "";
      return name + "_iface" + args;
    });
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
    const funcDecls = files.flatMap((file) => file.getFunctions());
    const funcDeclsByName = groupBy(funcDecls, (decl) => decl.getName());
    for (const [name, decls] of Object.entries(funcDeclsByName)) {
      output.push(
        ...renderSignatureGroup(
          this.overloadGroupToPython(
            name,
            decls.map((x) => x.getSignature()),
          ),
          false,
        ),
      );
    }
    let next: Needed | undefined;
    while ((next = popElt(this.neededSet))) {
      if (next.type === "ident") {
        let res = this.convertNeededIdent(next.ident);
        if (res) {
          output.push(res);
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
          output.push(res);
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
    output.splice(1, 0, typevarDecls);
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

  convertNeededIdent(ident: Identifier): string | undefined {
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
        );
        return `${name} = ${renderedType}`;
      case "varDecl":
        console.warn("Skipping varDecl", ident.getText());
    }
    return undefined;
  }

  renderSimpleDecl(name: string, typeNode: TypeNode) {
    const renderedType = this.typeToPython(typeNode, false);
    return renderSimpleDeclaration(name, renderedType);
  }

  convertVarDecl(varDecl: VariableDeclaration): string | undefined {
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
        return renderSimpleDeclaration(name, "Window");
      }
      console.warn("intersection varDecl:", varDecl.getText());
      return undefined;
    }
    return this.renderSimpleDecl(name, typeNode);
  }

  convertVarDeclOfReferenceType(name: string, typeNode: TypeReferenceNode) {
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
      return this.renderSimpleDecl(name, typeNode);
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
      return renderSimpleDeclaration(name, classified.decl.getName());
    }
    if (classified.kind === "typeAlias") {
      return this.renderSimpleDecl(name, classified.decl.getTypeNode());
    }
    assertUnreachable(classified);
  }

  convertMembersDeclaration(
    name: string,
    type: { getMembers: TypeLiteralNode["getMembers"] },
    bases = [],
    typeParams: string[],
  ): string {
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
        const pyType = this.typeToPython(
          param.getTypeNode()!,
          optional,
        ).replace(/^JsArray/, "list");
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

  convertClass(decl: ClassDeclaration): string | undefined {
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
            ["classmethod"],
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
    return renderPyClass(name, supers, entries.join("\n"));
  }

  convertInterface(
    name: string,
    supers: string[],
    members: TypeElementTypes[],
    staticMembers: TypeElementTypes[],
    typeParams: string[],
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
    if (typeParams.length > 0) {
      const typeParamsList = Array.from(new Set(typeParams)).join(",");
      supers.push(`Generic[${typeParamsList}]`);
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
      .flatMap((gp) => renderSignatureGroup(gp, true));
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
    if (
      isOptional &&
      !Node.isUnionTypeNode(typeNode) &&
      !typeNode.getType().isAny()
    ) {
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
    if (typeText === "never") {
      return "Never";
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
      return `JsArray[${eltType}]`;
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
      const ident = typeNode.getTypeName();
      let name = ident.getText();
      if (typeNode.getType().isTypeParameter()) {
        this.typeParams.add(name);
        return name;
      }
      const args = getExpressionTypeArgs(ident, typeNode)
        .map((ty) => this.typeToPython(ty, false))
        .join(", ");
      let fmtArgs = "";
      if (args) {
        fmtArgs = `[${args}]`;
      }

      if (
        name.startsWith("Intl") ||
        ["console.ConsoleConstructor", "NodeJS.CallSite", "FlatArray"].includes(
          name,
        )
      ) {
        return "Any";
      }
      if (["Exclude", "Readonly"].includes(name)) {
        return args[0];
      }
      if (name === "URL") {
        return "URL_";
      }
      if (name === "Function") {
        return "Callable[..., Any]";
      }
      if (name === "Promise") {
        name = "Future";
      } else if (
        !typeNode.getType().isTypeParameter() &&
        !this.convertedSet.has(name)
      ) {
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

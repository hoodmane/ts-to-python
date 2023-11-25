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
} from "./render.ts";

import { groupBy, groupByGen, WrappedGen, split, popElt } from "./groupBy.ts";

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
`
  .trim();

class Converter {
  project: Project;
  convertedSet: Set<string>;
  neededSet: Set<string>;
  ifaceDecls: Record<string, InterfaceDeclaration[]>;
  constructor() {
    this.project = new Project({libFolderPath: "./node_modules/typescript/lib",});
    this.project.addSourceFilesAtPaths("a.ts");
    this.convertedSet = new Set(["Iterable", "Iterator", "IterableIterator"]);
    this.neededSet = new Set();
  }

  emit(allFiles: boolean) {
    console.warn({allFiles});
    let varDecls: VariableDeclaration[];
    let ifaces: InterfaceDeclaration[];
    if (allFiles) {
      const files = this.project.resolveSourceFileDependencies();
      varDecls = files.flatMap((file) => file.getVariableDeclarations());
      ifaces = files.flatMap((file) => file.getInterfaces());
    } else {
      const file = this.project.getSourceFile("a.ts")!;
      varDecls = file.getVariableDeclarations();
      ifaces = file.getInterfaces();
    }
    this.ifaceDecls = groupBy(ifaces, (item) => item.getName());

    const output: string[] = [IMPORTS];
    for (const varDecl of varDecls) {
      const typeNode = varDecl.getTypeNode()!;
      if (!typeNode) {
        continue;
      }
      const name = varDecl.getName();
      if (this.convertedSet.has(name)) {
        continue;
      }
      this.convertedSet.add(name);
      let typeLiteral = typeNode.asKind(SyntaxKind.TypeLiteral);
      if (typeLiteral) {
        output.push(this.convertVarDecl(name, typeLiteral));
        continue;
      }
      let typeRef = typeNode.asKind(SyntaxKind.VariableDeclaration);
      if (typeRef) {
        //   console.warn(this.interfaceDecls[varDecl.type.typeName.getText()])
        //   const ty = this.checker.getTypeFromTypeNode(varDecl.type) as ts.TypeReference;
        //   ts.factory;
        //   console.warn("node:", ty.node);
        //   // const ty = this.checker.Reference(varDecl.type) as ts.TypeReference;
        //   console.warn(this.checker.typeToString(ty));
        //   continue;
      }
      console.warn("ignored varDecl:", name);
      // output.push()
    }
    let name: string | undefined;
    while (name = popElt(this.neededSet)) {
      if (this.convertedSet.has(name)) {
        continue;
      }
      this.convertedSet.add(name);
      if ((name in this.ifaceDecls) ) {
        const decls = this.ifaceDecls[name];
        output.push(this.convertInterface(name, [], decls.flatMap(node => node.getMembers())));
        continue;
      }
      // if (name in typeDecls) {
      //   const typeDecl = typeDecls[name][0];
      //   const type = this.typeToPython(typeDecl.type, false);
      //   output.push(`${name} = ${type}`);
      //   continue;
      // }
      // if (name in namespaceDecls) {
      //   console.warn("TODO namespace decl", name);
      //   continue;
      // }
      console.warn(`Can't find ${name}`);
    }

    console.log(output.join("\n\n"));
  }

  convertVarDecl(name: string, type: TypeLiteralNode): string {
    type.getMembers();

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
    let supers: string[] = [];
    let members: TypeElementTypes[] = [];
    if (prototype) {
      if (prototype.length > 1) {
        throw new Error("Didn't expect to see multiple prototype fields...");
      }
      const proto = prototype[0] as PropertySignature;
      const typeNode = proto.getTypeNode();
      if (typeNode?.isKind(SyntaxKind.TypeReference)) {
        const protoname = typeNode.getTypeName().getText();
        if (protoname === name) {
          const declArray = this.ifaceDecls[name];
          members = declArray.flatMap((node) => node.getMembers());
        }
      } else {
        throw new Error("Excepted prototype type to be TypeReference");
      }
    }
    return this.convertInterface(name, supers, members, staticMembers);
  }

  convertSignatures(
    sigs: readonly Signature[],
    topLevelName?: string,
  ): string {
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

  convertSignature(
    sig: Signature,
    topLevelName?: string,
  ): string {
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
      throw e;
      console.warn("failed to convert", sig.compilerSignature.declaration?.getText());
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
    type: TypeNode,
    isOptional: boolean,
    topLevelName?: string,
  ): string {
    let inner = this.typeToPythonInner(type, isOptional, topLevelName);
    if (isOptional && type.getType().isUnion()) {
      inner += " | None";
    }
    return inner;
  }

  unionTypeNodeToPython(typeNode: UnionTypeNode, isOptional: boolean): string {
    const unionTypes = typeNode.getTypeNodes() as TypeNode[];
    const [literals, rest] = split(unionTypes, Node.isLiteralTypeNode);
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
    if (type.isNumber()) {
      return "int | float";
    }
    // @ts-ignore
    if (type._hasTypeFlag(TypeFlags.BigInt)) {
      return "int";
    }
    if (type.isBoolean()) {
      return "bool";
    }
    if (type.isString()) {
      return "str";
    }
    if (type.isVoid() || type.isUndefined() || type.isNull()) {
      return "None";
    }
    // @ts-ignore
    if (type._hasTypeFlag(TypeFlags.ESSymbol)) {
      return "Symbol";
    }
    if (type.getFlags() & (ts.TypeFlags.Any | ts.TypeFlags.Unknown)) {
      return "Any";
    }
    if (type.getFlags() & ts.TypeFlags.NonPrimitive) {
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
            !(Node.isThisTypeNode(type) && type.getText() === "ThisType"),
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
      const args = typeNode
        .getTypeArguments()
        .map((ty) => this.typeToPython(ty, false))
        .join(", ");
      let fmtArgs = "";
      if (args) {
        fmtArgs = `[${args}]`;
      }
      let name = typeNode.getTypeName().getText();
      if (name === "Promise") {
        name = "Future";
      } else if (name === "Function") {
        name = "Callable";
      } else if (!type.isTypeParameter() && !this.convertedSet.has(name)) {
        this.neededSet.add(name);
      }
      return `${name}${fmtArgs}`;
    }
    if (Node.isTypeOperatorTypeNode(typeNode)) {
      // Just ignore readonly
      const operator = typeNode.getOperator();
      if (
        [ts.SyntaxKind.ReadonlyKeyword, ts.SyntaxKind.UniqueKeyword].includes(
          operator,
        )
      ) {
        return this.typeToPython(typeNode.getTypeNode(), false);
      }
      // throw new Error("Unknown type operator " + operator);
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
    console.warn(typeNode.getKindName());
    const sf = typeNode.getSourceFile();
    const { line, column } = sf.getLineAndColumnAtPos(typeNode.getStart());
    console.warn(
      `No known conversion for '${type.getText()}'\n ${sf.getFilePath()}:${line}:${column}`,
    );
    return "___A";
  }
}

new Converter().emit(!!process.argv[2]);

import * as ts from "typescript";
import {
  renderSignatureGroup,
  renderSignature,
  renderInnerSignature,
  renderProperty,
  renderPyClass,
  PySig,
  PyParam,
  PySigGroup,
} from "./render.ts";
import { groupBy, groupByGen, WrappedGen } from "./groupBy.ts";

function formatSyntaxKind(k: ts.Node) {
  // @ts-ignore
  return Debug.formatSyntaxKind(k.kind);
}

function idText(id: ts.Identifier | ts.BindingName) {
  return (id as ts.Identifier).text || id.getText();
}

type SyntaxKindMap = {
  [ts.SyntaxKind.VariableStatement]: ts.VariableStatement;
  [ts.SyntaxKind.VariableDeclaration]: ts.VariableDeclaration;
  [ts.SyntaxKind.PropertySignature]: ts.PropertySignature;
  [ts.SyntaxKind.MethodSignature]: ts.MethodSignature;
  [ts.SyntaxKind.ConstructSignature]: ts.ConstructSignatureDeclaration;
  [ts.SyntaxKind.InterfaceDeclaration]: ts.InterfaceDeclaration;
};
type GroupedBySyntaxKind = { [K in keyof SyntaxKindMap]?: SyntaxKindMap[K][] };

function groupBySyntaxKind(list: Iterable<ts.Node>): GroupedBySyntaxKind {
  const gen = groupBySyntaxKindGen();
  for (const x of list) {
    gen.next(x);
  }
  return gen.done();
}

function groupBySyntaxKindGen(): WrappedGen<ts.Node, GroupedBySyntaxKind> {
  return groupByGen<ts.Node, any>((node) => {
    return node.kind;
  });
}

function filterSignatures(
  name: string,
  signatures: ts.SignatureDeclaration[],
): ts.SignatureDeclaration[] {
  if (["assign", "fromEntries"].includes(name)) {
    signatures = signatures.filter(
      (sig) => !sig.typeParameters || sig.typeParameters.length === 0,
    );
  }
  if (name === "getOwnPropertyDescriptors") {
    const anyNode = ts.factory.createKeywordTypeNode(ts.SyntaxKind.AnyKeyword);
    // @ts-ignore
    signatures[0].type = anyNode;
  }
  if (name === "bind") {
    const anyNode = ts.factory.createKeywordTypeNode(ts.SyntaxKind.AnyKeyword);
    const thisArgId = ts.factory.createIdentifier("thisArg");
    const thisArg = ts.factory.createParameterDeclaration(
      undefined,
      undefined,
      thisArgId,
      undefined,
      anyNode,
    );
    const decl = ts.factory.createMethodSignature(
      undefined,
      "bind",
      undefined,
      undefined,
      [thisArg],
      anyNode,
    );
    return [decl];
  }
  return signatures;
}

function groupMembers(members: ts.TypeElement[]): {
  methods: Record<string, ts.SignatureDeclaration[]>;
  properties: ts.PropertySignature[];
  constructors: ts.ConstructSignatureDeclaration[];
} {
  const grouped = groupBySyntaxKind(members);
  const allProperties = grouped[ts.SyntaxKind.PropertySignature] || [];
  const { functions = [], properties = [] } = groupBy(allProperties, (prop) =>
    ts.isFunctionTypeNode(prop.type!) ? "functions" : "properties",
  );
  const methodSigs = grouped[ts.SyntaxKind.MethodSignature] || [];
  const empty: [string, ts.FunctionTypeNode | ts.MethodSignature][] = [];
  const methodOrFuncProps = empty.concat(
    methodSigs.map((meth) => [meth.name.getText(), meth]),
    functions.map((func) => [
      func.name.getText(),
      func.type as ts.FunctionTypeNode,
    ]),
  );
  const methodNamePairs = groupBy(methodOrFuncProps, ([name, prop]) => name);
  const methods = Object.fromEntries(
    Object.entries(methodNamePairs).map(([name, v]) => [
      name,
      v.map(([_, prop]) => prop),
    ]),
  );
  const constructors = grouped[ts.SyntaxKind.ConstructSignature] || [];
  return { methods, properties, constructors };
}

const IMPORTS = `
from collections.abc import Callable
from typing import overload, Any, Literal
`
  .trim()
  .split("\n");

class Converter {
  convertedSet: Set<string>;
  neededSet: Set<string>;
  program: ts.Program;
  checker: ts.TypeChecker;
  origFile: ts.SourceFile;
  interfaceDecls: Record<string, ts.InterfaceDeclaration[]>;

  constructor(file: string) {
    this.convertedSet = new Set();
    this.neededSet = new Set();
    this.program = ts.createProgram([file], {});
    this.origFile = this.program.getSourceFile(file)!;
    this.checker = this.program.getTypeChecker();
    this.interfaceDecls = {};
  }

  /**
   * Prints out particular nodes from a source file
   *
   * @param file a path to a file
   * @param identifiers top level identifiers available
   */
  extract(): void {
    // Create a Program to represent the project, then pull out the
    // source file to parse its AST.

    //   console.log(program.getSourceFiles().map(file => file.fileName));

    // To print the AST, we'll use TypeScript's printer
    const printer = ts.createPrinter({ newLine: ts.NewLineKind.LineFeed });

    const groupGen = groupBySyntaxKindGen();
    // this.origFile.forEachChild(groupGen.next);
    this.program.getSourceFiles().forEach((sourceFile) => sourceFile.forEachChild(groupGen.next));
    const grouped = groupGen.done();
    const output: string[] = [];
    output.push(...IMPORTS);

    this.interfaceDecls = groupBy(
      grouped[ts.SyntaxKind.InterfaceDeclaration] || [],
      (node) => node.name.getText(),
    );
    // for (const [name, declArray] of Object.entries(interfaceDecls)) {
    //   output.push(convertInterface(checker, name + "_interface", [], declArray.flatMap(node => node.members)));
    // }
    const varDecls = (grouped[ts.SyntaxKind.VariableStatement] || []).flatMap(
      (v) => {
        const decls: ts.VariableDeclaration[] = [];
        v.declarationList.forEachChild((x) =>
          decls.push(x as ts.VariableDeclaration),
        );
        return decls;
      },
    );
    for (const varDecl of varDecls) {
      if (!varDecl.type) {
        continue;
      }
      if (ts.isTypeLiteralNode(varDecl.type)) {
        output.push(this.convertTypeLiteralVarDecl(varDecl));
        continue;
      }
      // output.push()
    }

    console.log(output.join("\n\n"));
  }

  convertTypeLiteralVarDecl(varDecl: ts.VariableDeclaration): string {
    if (!varDecl.type || !ts.isTypeLiteralNode(varDecl.type)) {
      throw new Error("Assertion error");
    }
    const declName = varDecl.name.getText();
    const { prototype, staticMembers } = groupBy(varDecl.type.members, (m) => {
      if (ts.isPropertySignature(m) && m.name.getText() === "prototype") {
        return "prototype";
      } else {
        return "staticMembers";
      }
    });
    let supers: string[] = [];
    let members: ts.TypeElement[] = [];
    if (prototype) {
      if (prototype.length > 1) {
        throw new Error("Didn't expect to see multiple prototype fields...");
      }
      const proto = prototype[0] as ts.PropertySignature;
      if (ts.isTypeReferenceNode(proto.type!)) {
        const protoname = proto.type.typeName.getText();
        if (protoname === declName) {
          const declArray = this.interfaceDecls[declName];
          members = declArray.flatMap((node) => node.members);
        }
      } else {
        throw new Error("Excepted prototype type to be TypeReference");
      }
    }
    return this.convertInterface(declName, supers, members, staticMembers);
  }

  convertSignatures(
    sigs: readonly ts.Signature[],
    topLevelName?: string,
  ): string {
    const converted = sigs.map((sig) =>
      this.convertSignature(sig.getDeclaration(), topLevelName),
    );
    if (!topLevelName) {
      return converted.join(" | ");
    }

    if (converted.length === 1) {
      return converted[0];
    }
    return converted.map((x) => "@overload\n" + x).join("\n\n");
  }

  sigToPython(sig: ts.SignatureDeclaration, decorators: string[] = []): PySig {
    try {
      const params = sig.parameters.map((param) => {
        const spread = !!param.dotDotDotToken;
        const optional = !!param.questionToken;
        const pyType = this.typeToPython(param.type!, optional);
        return { name: idText(param.name), pyType, optional, spread };
      });
      const retNode = sig.type!;
      const returns = this.typeToPython(retNode, false);
      return { params, returns, decorators };
    } catch (e) {
      throw e;
      console.warn("failed to convert", sig.getText());
    }
  }

  overloadGroupToPython(
    name: string,
    signatures: (ts.SignatureDeclaration | ts.FunctionTypeNode)[],
    decorators: string[] = [],
  ): PySigGroup {
    signatures = filterSignatures(name, signatures);
    const sigs = signatures.map((sig) => this.sigToPython(sig, decorators));
    return { name, sigs };
  }

  convertSignature(
    sig: ts.SignatureDeclaration,
    topLevelName?: string,
  ): string {
    const pySig = this.sigToPython(sig);
    if (topLevelName) {
      return renderSignature(topLevelName, pySig);
    }
    return renderInnerSignature(pySig);
  }

  convertPropertySignature(member: ts.PropertySignature, isStatic: boolean = false): string {
    const memberName = member.name.getText();
    const isOptional = !!member.questionToken;
    const pytype = this.typeToPython(member.type!, isOptional, memberName);
    let readOnly = false;
    for (const mod of member.modifiers || []) {
      if (mod.getText() === "readonly") {
        readOnly = true;
      }
    }
    return renderProperty(memberName, pytype, readOnly, isStatic);
  }

  convertInterface(
    name: string,
    supers: string[],
    members: ts.TypeElement[],
    staticMembers: ts.TypeElement[] = [],
  ) {
    const { methods, properties } = groupMembers(members);
    const { methods: staticMethods, properties: staticProperties, constructors } =
      groupMembers(staticMembers);
    for (const key of Object.keys(staticMethods)) {
      delete methods[key];
    }

    const overloadGroups = Object.entries(methods)
      .filter(([name]) => !name.includes("["))
      .map(([name, sigs]) => this.overloadGroupToPython(name, sigs));
    if (constructors) {
      staticMethods["new"] = constructors;
    }
    const staticOverloadGroups = Object.entries(staticMethods)
      .filter(([name]) => !name.includes("["))
      .map(([name, sigs]) =>
        this.overloadGroupToPython(name, sigs, ["classmethod"]),
      );
    const renderedProps = properties.map((prop) =>
      this.convertPropertySignature(prop),
    );
    const renderedStaticProps = staticProperties.map(prop => this.convertPropertySignature(prop, true));
    const pyMethods = overloadGroups
      .concat(staticOverloadGroups)
      .flatMap((gp) => renderSignatureGroup(gp));
    const entries = renderedProps.concat(renderedStaticProps, pyMethods);
    return renderPyClass(name, supers, entries.join("\n"));
  }

  typeToPython(
    type: ts.TypeNode,
    isOptional: boolean,
    topLevelName?: string,
  ): string {
    let inner = this.typeToPythonInner(type, isOptional, topLevelName);
    if (isOptional && !ts.isUnionTypeNode(type)) {
      inner += " | None";
    }
    return inner;
  }

  typeToPythonInner(
    typeNode: ts.TypeNode,
    isOptional: boolean,
    topLevelName?: string,
  ): string {
    const type = this.checker.getTypeFromTypeNode(typeNode);
    if (type.getFlags() & ts.TypeFlags.Number) {
      return "int | float";
    }
    if (type.getFlags() & ts.TypeFlags.BigInt) {
      return "int";
    }
    if (type.getFlags() & ts.TypeFlags.Boolean) {
      return "bool";
    }
    if (type.getFlags() & ts.TypeFlags.String) {
      return "str";
    }
    if (
      type.getFlags() &
      (ts.TypeFlags.Void | ts.TypeFlags.Undefined | ts.TypeFlags.Null)
    ) {
      return "None";
    }
    if (type.getFlags() & ts.TypeFlags.ESSymbol) {
      return "Symbol";
    }
    if (type.getFlags() & (ts.TypeFlags.Any | ts.TypeFlags.Unknown)) {
      return "Any";
    }
    if (type.getFlags() & ts.TypeFlags.NonPrimitive) {
      // "object"...
      return "Any";
    }
    if (ts.isThisTypeNode(typeNode)) {
      return "Self";
    }
    if (ts.isUnionTypeNode(typeNode)) {
      const literals = typeNode.types.filter(ts.isLiteralTypeNode);
      const rest = typeNode.types.filter((node) => !ts.isLiteralTypeNode(node));
      const types = rest.map((ty) => this.typeToPython(ty, false));
      if (literals.length > 0) {
        const lits = literals.map((lit) => lit.getText()).join(", ");
        types.push(`Literal[${lits}]`);
      }
      if (isOptional) {
        types.push("None");
      }
      return types.join(" | ");
    }
    if (ts.isParenthesizedTypeNode(typeNode)) {
      const ty = this.typeToPython(typeNode.type, false);
      return `(${ty})`;
    }
    if (ts.isIntersectionTypeNode(typeNode)) {
      const filteredTypes = typeNode.types.filter(
        (type) =>
          !(
            ts.isTypeReferenceNode(type) &&
            type.typeName.getText() === "ThisType"
          ),
      );
      if (filteredTypes.length === 1) {
        return this.typeToPython(filteredTypes[0], false);
      }
      const typeString = this.checker.typeToString(type);
      if (typeString === "Window & typeof globalThis") {
        return "Any";
      }
      if (typeString === "ArrayBufferLike & { BYTES_PER_ELEMENT?: never; }") {
        return "ArrayBuffer";
      }
    }
    if (ts.isTypeLiteralNode(typeNode)) {
      // return checker.typeToString(type);
      return "Any";
    }
    if (ts.isLiteralTypeNode(typeNode)) {
      return `Literal[${typeNode.getText()}]`;
    }
    if (type.getCallSignatures().length > 0) {
      return this.convertSignatures(type.getCallSignatures(), topLevelName);
    }
    if (ts.isArrayTypeNode(typeNode)) {
      const eltType = this.typeToPython(typeNode.elementType, false);
      return `list[${eltType}]`;
    }
    if (ts.isTupleTypeNode(typeNode)) {
      let elts = typeNode.elements
        .map((elt) => this.typeToPython(elt, false))
        .join(", ");
      if (elts === "") {
        elts = "()";
      }
      return `tuple[${elts}]`;
    }
    if (ts.isTypeReferenceNode(typeNode)) {
      const args = typeNode.typeArguments
        ?.map((ty) => this.typeToPython(ty, false))
        .join(", ");
      let fmtArgs = "";
      if (args) {
        fmtArgs = `[${args}]`;
      }
      let name = typeNode.typeName.getText();
      if (name === "Promise") {
        name = "Future";
      } else if (name === "Function") {
        name = "Callable";
      } else if (!type.isTypeParameter() && !this.convertedSet.has(name)) {
        this.neededSet.add(name);
      }
      return `${typeNode.typeName.getText()}${fmtArgs}`;
    }
    if (ts.isTypeOperatorNode(typeNode)) {
      // Just ignore readonly
      const operator = typeNode.operator;
      if (
        [ts.SyntaxKind.ReadonlyKeyword, ts.SyntaxKind.UniqueKeyword].includes(
          operator,
        )
      ) {
        return this.typeToPython(typeNode.type, false);
      }
      // throw new Error("Unknown type operator " + operator);
    }
    if (ts.isTemplateLiteralTypeNode(typeNode)) {
      return "str";
    }
    if (ts.isConstructorTypeNode(typeNode)) {
      return "Any";
    }
    if (ts.isMappedTypeNode(typeNode)) {
      return "Any";
    }
    if (ts.isIndexedAccessTypeNode(typeNode)) {
      return "Any";
    }
    if (ts.isTypeQueryNode(typeNode)) {
      return "Any";
    }
    console.warn(formatSyntaxKind(typeNode));
    const sf = typeNode.getSourceFile();
    const { line, character } = sf.getLineAndCharacterOfPosition(
      typeNode.getStart(),
    );
    console.warn(
      `No known conversion for '${this.checker.typeToString(type)}'\n ${
        sf.fileName
      }:${line + 1}:${character + 1}`,
    );
    return "___A";
  }
}

// Run the extract function with the script's arguments
new Converter(process.argv[2]).extract();

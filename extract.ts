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

function formatSyntaxKind(k: ts.Node) {
  // @ts-ignore
  return Debug.formatSyntaxKind(k.kind);
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

function groupBy<T, K extends keyof any>(
  list: Iterable<T>,
  getKey: (item: T) => K,
) {
  const gen = groupByGen(getKey);
  for (const x of list) {
    gen.next(x);
  }
  return gen.done();
}

type WrappedGen<T, R> = { next: (T) => void; done: () => R };

function groupBySyntaxKindGen(): WrappedGen<ts.Node, GroupedBySyntaxKind> {
  return groupByGen<ts.Node, any>((node) => {
    return node.kind;
  });
}

function groupByGen<T, K extends keyof any>(
  getKey: (item: T) => K,
): WrappedGen<T, Record<K, T[]>> {
  const g = groupByGenHelper(getKey);
  g.next();
  return {
    next: (x) => {
      g.next(x);
    },
    done: () => g.return(undefined as any).value,
  };
}

function* groupByGenHelper<T, K extends keyof any>(getKey: (item: T) => K) {
  const result = {} as Record<K, T[]>;
  try {
    while (true) {
      const currentItem: T = yield result;
      const group = getKey(currentItem);
      if (!result[group]) {
        result[group] = [];
      }
      result[group].push(currentItem);
    }
  } finally {
    return result;
  }
}

/**
 * Prints out particular nodes from a source file
 *
 * @param file a path to a file
 * @param identifiers top level identifiers available
 */
function extract(file: string, identifiers: string[]): void {
  // Create a Program to represent the project, then pull out the
  // source file to parse its AST.
  let program = ts.createProgram([file], { allowJs: true });
  let checker = program.getTypeChecker();
  //   console.log(program.getSourceFiles().map(file => file.fileName));
  const sourceFile = program.getSourceFile(file)!;

  // To print the AST, we'll use TypeScript's printer
  const printer = ts.createPrinter({ newLine: ts.NewLineKind.LineFeed });

  const groupGen = groupBySyntaxKindGen();
  sourceFile.forEachChild(groupGen.next);
  const grouped = groupGen.done();
  const output: string[] = [];

  const interfaceDecls = groupBy(
    grouped[ts.SyntaxKind.InterfaceDeclaration] || [],
    (node) => node.name.getText(),
  );
  for (const [name, declArray] of Object.entries(interfaceDecls)) {
    output.push(convertInterface(checker, name + "_interface", [], declArray.flatMap(node => node.members)));
  }
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
      output.push(convertTypeLiteralVarDecl(checker, varDecl));
      continue;
    }
    // output.push()
  }

  console.log(output.join("\n\n"));
}

function convertTypeLiteralVarDecl(
  checker: ts.TypeChecker,
  varDecl: ts.VariableDeclaration,
): string {
  if (!varDecl.type || !ts.isTypeLiteralNode(varDecl.type)) {
    throw new Error("Assertion error");
  }
  const members = groupBy(varDecl.type.members, (m) => {
    if (ts.isPropertySignature(m) && m.name.getText() === "prototype") {
      return "prototype";
    } else {
      return "rest";
    }
  });
  let supers : string[] = [];
  if (members.prototype) {
    if (members.prototype.length > 1) {
      throw new Error("Didn't expect to see multiple prototype fields...");
    }
    const proto = members.prototype[0] as ts.PropertySignature;
    if (ts.isTypeReferenceNode(proto.type!)) {
      supers.push(proto.type.typeName.getText() + "_interface");
    } else {
      throw new Error("Excepted prototype type to be TypeReference");
    }
  }
  return convertInterface(checker, varDecl.name.getText(), supers, members.rest);
}

function convertVariableStatement(
  checker: ts.TypeChecker,
  node: ts.VariableStatement,
) {
  const printer = ts.createPrinter({ newLine: ts.NewLineKind.LineFeed });

  const declNode = node.declarationList.declarations[0];
  const typeNode = declNode.getChildren().at(-1);
  if (!typeNode || !ts.isTypeNode(typeNode)) {
    throw new Error("oops??");
  }
  if (ts.isTypeLiteralNode(typeNode)) {
  }
  return;

  // console.log(a.map((k) => Debug.formatSyntaxKind(k.kind)));
  // const constructSig = convertSignatures(
  //   checker,
  //   declNode,
  //   type.getConstructSignatures(),
  //   "new",
  // );
  // let proto;
  // for (let prop of type.getProperties()) {
  //   if (prop.name === "prototype") {
  //     proto = prop;
  //   }
  //   // const type = checker.symbol
  //   // let a: TypeElement = checker.getAliasedSymbol()
  // }

  // console.log(
  //   "new",
  //   checker.signatureToString(type.getConstructSignatures()[0]),
  // );
  // console.log(type.getProperties().map((x) => x.name));
}

function convertSignatures(
  checker: ts.TypeChecker,
  sigs: readonly ts.Signature[],
  topLevelName?: string,
): string {
  const converted = sigs.map((sig) =>
    convertSignature(checker, sig.getDeclaration(), topLevelName),
  );
  if (!topLevelName) {
    return converted.join(" | ");
  }

  if (converted.length === 1) {
    return converted[0];
  }
  return converted.map((x) => "@overload\n" + x).join("\n\n");
}

function sigToPython(
  checker: ts.TypeChecker,
  sig: ts.SignatureDeclaration,
  decorators: string[] = []
): PySig {
  const params = sig.parameters.map((param) => {
    const optional = !!param.questionToken;
    const pyType = typeToPython(checker, param.type!, optional);
    return { name: param.name.getText(), pyType, optional };
  });
  const retNode = sig.type!;
  const returns = typeToPython(checker, retNode, false);
  return { params, returns, decorators };
}

function overloadGroupToPython(
  checker,
  name: string,
  signatures: ts.SignatureDeclaration[],
  decorators: string[] = []
): PySigGroup {
  const sigs = signatures.map(sig => sigToPython(checker, sig, decorators));
  return {name, sigs};
}

function convertSignature(
  checker: ts.TypeChecker,
  sig: ts.SignatureDeclaration,
  topLevelName?: string,
): string {
  const pySig = sigToPython(checker, sig);
  if (topLevelName) {
    return renderSignature(topLevelName, pySig);
  }
  return renderInnerSignature(pySig);
}

function convertPropertySignature(
  checker: ts.TypeChecker,
  member: ts.PropertySignature,
): string {
  const memberName = member.name.getText();
  const isOptional = !!member.questionToken;
  const pytype = typeToPython(
    checker,
    member.type!,
    isOptional,
    memberName,
  );
  let readOnly = false;
  for (const mod of member.modifiers || []) {
    if (mod.getText() === "readonly") {
      readOnly = true;
    }
  }
  const isDef = pytype.includes("def");
  if (readOnly && !isDef) {
    return renderProperty(memberName, pytype);
  }
  if (isDef) {
    return pytype;
  }
  return `${memberName}: ${pytype}`;
}


function convertInterface(
  checker: ts.TypeChecker,
  name: string,
  supers: string[],
  members: ts.TypeElement[],
) {
  const grouped = groupBySyntaxKind(members);
  const properties = (grouped[ts.SyntaxKind.PropertySignature] || []).map((prop) =>
    convertPropertySignature(checker, prop),
  );
  const methods = groupBy((grouped[ts.SyntaxKind.MethodSignature] || []), prop => prop.name.getText());
  const overloadGroups = Object.entries(methods).map(([name, sigs]) =>
    overloadGroupToPython(checker, name, sigs)
  );
  if (grouped[ts.SyntaxKind.ConstructSignature]) {
    overloadGroups.push(overloadGroupToPython(checker, "new", grouped[ts.SyntaxKind.ConstructSignature], ["classmethod"]));
  }
  const pyMethods = overloadGroups.flatMap(gp => renderSignatureGroup(gp));
  const entries = properties.concat(pyMethods);
  return renderPyClass(name, supers, entries.join("\n"));
}

function typeToPython(
  checker: ts.TypeChecker,
  type: ts.TypeNode,
  isOptional: boolean,
  topLevelName?: string,
): string {
  let inner = typeToPythonInner(checker, type, isOptional, topLevelName);
  if (isOptional && !ts.isUnionTypeNode(type)) {
    inner += " | None";
  }
  return inner;
}

function typeToPythonInner(
  checker: ts.TypeChecker,
  typeNode: ts.TypeNode,
  isOptional: boolean,
  topLevelName?: string,
): string {
  const type = checker.getTypeFromTypeNode(typeNode);
  if (type.getFlags() & ts.TypeFlags.Number) {
    return "int | float";
  }
  if (type.getFlags() & ts.TypeFlags.Boolean) {
    return "bool";
  }
  if (type.getFlags() & ts.TypeFlags.String) {
    return "str";
  }
  if (type.getFlags() & ts.TypeFlags.Void) {
    return "None";
  }
  if (ts.isUnionTypeNode(typeNode)) {
    const types = typeNode.types.map(
      (ty) => "(" + typeToPython(checker, ty, false) + ")",
    );
    if (isOptional) {
      types.push("None");
    }
    return types.join(" | ");
  }
  if (type.getCallSignatures().length > 0) {
    return convertSignatures(checker, type.getCallSignatures(), topLevelName);
  }
  if (ts.isTypeReferenceNode(typeNode)) {
    const args = typeNode.typeArguments?.map(ty => typeToPython(checker, ty, false)).join(", ");
    let fmtArgs = "";
    if (args) {
      fmtArgs = `[${args}]`;
    }
    if (typeNode.typeName.getText() === "Promise") {
      return `Future${fmtArgs}`;
    }
    return `${typeNode.typeName.getText()}${fmtArgs}`;
  }
  throw new Error("No known conversion for " + checker.typeToString(type))
}

// Run the extract function with the script's arguments
extract(process.argv[2], process.argv.slice(3));

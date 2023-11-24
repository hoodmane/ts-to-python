import * as ts from "typescript";

type SyntaxKindMap = {
  [ts.SyntaxKind.VariableDeclaration]: ts.VariableDeclaration;
  [ts.SyntaxKind.PropertySignature]: ts.PropertySignature;
  [ts.SyntaxKind.MethodSignature]: ts.MethodSignature;
  [ts.SyntaxKind.ConstructSignature]: ts.ConstructSignatureDeclaration;
};
type GroupedBySyntaxKind = { [K in keyof SyntaxKindMap]?: SyntaxKindMap[K][] };

function groupBySyntaxKind(list: Iterable<ts.Node>): GroupedBySyntaxKind {
  return groupBy(list, (node) => node.kind) as unknown as GroupedBySyntaxKind;
}

function groupBy<T, K extends keyof any>(
  list: Iterable<T>,
  getKey: (item: T) => K,
) {
  const result = {} as Record<K, T[]>;
  for (let currentItem of list) {
    const group = getKey(currentItem);
    if (!result[group]) {
      result[group] = [];
    }
    result[group].push(currentItem);
  }
  return result;
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

  // To give constructive error messages, keep track of found and un-found identifiers
  const interfaceDecls: Map<string, ts.InterfaceDeclaration[]> = new Map();

  // Loop through the root AST nodes of the file
  ts.forEachChild(sourceFile, (node) => {
    if (!ts.isInterfaceDeclaration(node)) {
      return;
    }
    const name = node.name.getText();
    const declArray = interfaceDecls.get(name) || [];
    interfaceDecls.set(name, declArray);
    declArray.push(node);
  });

  //   ts.forEachChild(sourceFile, (node) => {
  //     // This is an incomplete set of AST nodes which could have a top level identifier
  //     // it's left to you to expand this list, which you can do by using
  //     // https://ts-ast-viewer.com/ to see the AST of a file then use the same patterns
  //     // as below
  //     if (ts.isFunctionDeclaration(node)) {
  //       name = node.name!.text;
  //       // Hide the method body when printing
  //       node.body = undefined;
  //     } else if (ts.isVariableStatement(node)) {
  //       name = node.declarationList.declarations[0].name.getText(sourceFile);
  //     } else if (ts.isInterfaceDeclaration(node)) {
  //       name = node.name.text;
  //     }
  //     if (!identifiers.includes(name)) {
  //       return;
  //     }

  //     if (ts.isVariableStatement(node)) {
  //       convertVariableStatement(checker, node);
  //     }

  //     const container = identifiers.includes(name) ? foundNodes : unfoundNodes;
  //     container.push([name, node]);
  //   });
  for (const declArray of interfaceDecls.values()) {
    console.log(convertInterface(checker, declArray));
  }
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

  console.log(a.map((k) => Debug.formatSyntaxKind(k.kind)));
  const constructSig = convertSignatures(
    checker,
    declNode,
    type.getConstructSignatures(),
    "new",
  );
  let proto;
  for (let prop of type.getProperties()) {
    if (prop.name === "prototype") {
      proto = prop;
    }
    // const type = checker.symbol
    // let a: TypeElement = checker.getAliasedSymbol()
  }

  console.log(
    "new",
    checker.signatureToString(type.getConstructSignatures()[0]),
  );
  console.log(type.getProperties().map((x) => x.name));
}

function convertSignatures(
  checker: ts.TypeChecker,
  node: ts.Node,
  sigs: readonly ts.Signature[],
  topLevelName?: string,
): string {
  const converted = sigs.map((sig) =>
    convertSignature(checker, node, sig.getDeclaration(), topLevelName),
  );
  if (!topLevelName) {
    return converted.join(" | ");
  }

  if (converted.length === 1) {
    return converted[0];
  }
  return converted.map((x) => "@overload\n" + x).join("\n\n");
}

function convertSignature(
  checker: ts.TypeChecker,
  node: ts.Node,
  sig: ts.SignatureDeclaration,
  topLevelName?: string,
): string {
  const pyParams = sig.parameters.map((param) => {
    const type = checker.getTypeFromTypeNode(param.type!);
    const isOptional = !!param.questionToken;
    const pyType = typeToPython(checker, param, type, isOptional);
    return { name: param.name.getText(), pyType, isOptional };
  });
  const retNode = sig.type!;
  const ret = checker.getTypeFromTypeNode(retNode);
  const retType = typeToPython(checker, node, ret, false);
  if (topLevelName) {
    const formattedParams = pyParams.map(({ name, pyType, isOptional }) => {
      const maybeDefault = isOptional ? "=None" : "";
      return `${name}: ${pyType}${maybeDefault}`;
    });
    formattedParams.unshift("self");
    formattedParams.push("/");
    const joinedParams = formattedParams.join(", ");
    return `def ${topLevelName}(${joinedParams}) -> ${retType}: ...`;
  }
  const paramTypes = pyParams.map(({ pyType }) => pyType);
  return `Callable[[${paramTypes.join(", ")}], ${retType}]`;
}

function convertPropertySignature(
  checker: ts.TypeChecker,
  member: ts.PropertySignature,
): string {
  const memberName = member.name.getText();
  const memberType = checker.getTypeAtLocation(member);
  const isOptional = !!member.questionToken;
  const pytype = typeToPython(
    checker,
    member,
    memberType,
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
    return `@property\ndef ${memberName}(self) -> ${pytype}: ...`;
  }
  if (isDef) {
    return pytype;
  }
  return `${memberName}: ${pytype}`;
}

type HasMembers = ts.Node & { members: ts.NodeArray<ts.TypeElement> };

function convertHasMembers(checker: ts.TypeChecker, node: HasMembers) {
  const grouped = groupBySyntaxKind(node.members);

  node.members[0].kind;

  const res = {
    properties: (grouped[ts.SyntaxKind.PropertySignature] || []).map((prop) =>
      convertPropertySignature(checker, prop),
    ),
    constructors: (grouped[ts.SyntaxKind.ConstructSignature] || []).map(
      (sig) => {
        const res = convertSignature(checker, node, sig, "new");
        return "@classmethod\n" + res;
      },
    ),
  };
  return res;
}

function convertHasMembersList(
  checker: ts.TypeChecker,
  nodes: Iterable<HasMembers>,
) {
  const entriesList: {
    properties: any[];
    constructors: string[];
  }[] = [];
  for (const node of nodes) {
    entriesList.push(convertHasMembers(checker, node));
  }
  return {
    properties: entriesList.flatMap((e) => e.properties),
    constructors: entriesList.flatMap((e) => e.constructors),
  };
}

function convertInterface(
  checker: ts.TypeChecker,
  nodes: ts.InterfaceDeclaration[],
) {
  const entriesMap = convertHasMembersList(checker, nodes);
  let convertedConstructorSigs = entriesMap.constructors.map(
    (s) => "@classmethod\n" + s,
  );
  if (convertedConstructorSigs.length > 1) {
    convertedConstructorSigs = convertedConstructorSigs.map(
      (s) => "@overload\n" + s,
    );
  }
  const entries = entriesMap.properties.concat(convertedConstructorSigs);
  const interfaceName = nodes[0].name.getText();
  const body = entries
    .join("\n")
    .split("\n")
    .map((e) => "    " + e)
    .join("\n");
  return `class ${interfaceName}:\n${body}`;
}

function typeToPython(
  checker: ts.TypeChecker,
  node: ts.Node,
  type: ts.Type,
  isOptional: boolean,
  topLevelName?: string,
): string {
  let inner = typeToPythonInner(checker, node, type, isOptional, topLevelName);
  if (isOptional && !type.isUnion()) {
    inner += " | None";
  }
  return inner;
}

function typeToPythonInner(
  checker: ts.TypeChecker,
  node: ts.Node,
  type: ts.Type,
  isOptional: boolean,
  topLevelName?: string,
): string {
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
  if (type.isUnion()) {
    const types = type.types.map(
      (ty) => "(" + typeToPython(checker, node, ty, false) + ")",
    );
    if (isOptional) {
      types.push("None");
    }
    return types.join(" | ");
  }
  if (type.getCallSignatures().length > 0) {
    return convertSignatures(
      checker,
      node,
      type.getCallSignatures(),
      topLevelName,
    );
  }
  return "A___";
  console.log("unknown", checker.typeToString(type));
}

// Run the extract function with the script's arguments
extract(process.argv[2], process.argv.slice(3));

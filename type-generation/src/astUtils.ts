import {
  ClassDeclaration,
  ConstructorDeclaration,
  ConstructSignatureDeclaration,
  EntityName,
  FunctionTypeNode,
  Identifier,
  ImplementedKindToNodeMappings,
  InterfaceDeclaration,
  MethodDeclaration,
  MethodSignature,
  Node,
  PropertyDeclaration,
  PropertySignature,
  Signature,
  SyntaxKind,
  TypeAliasDeclaration,
  TypeArgumentedNode,
  TypeNode,
  TypeParameterDeclaration,
} from "ts-morph";
import { WrappedGen, groupBy, groupByGen, split } from "./groupBy";
import { ClassifiedIdentifier, GroupedBySyntaxKind } from "./types";
import { logger } from "./logger";

export function assertUnreachable(_value: never): never {
  throw new Error("Statement should be unreachable");
}

export function getNodeLocation(node: Node): string {
  const sf = node.getSourceFile();
  const { line, column } = sf.getLineAndColumnAtPos(node.getStart());
  return `${sf.getFilePath()}:${line}:${column}`;
}

export function groupBySyntaxKind(list: Iterable<Node>): GroupedBySyntaxKind {
  const gen = groupBySyntaxKindGen();
  for (const x of list) {
    gen.next(x);
  }
  return gen.done();
}

export function groupBySyntaxKindGen(): WrappedGen<Node, GroupedBySyntaxKind> {
  return groupByGen<Node, any>((node) => {
    return node.getKind();
  });
}

function getGroupedTypes<
  T1 extends keyof ImplementedKindToNodeMappings,
  T2 extends keyof ImplementedKindToNodeMappings,
>(
  grouped: GroupedBySyntaxKind,
  k1: T1,
  k2: T2,
): (ImplementedKindToNodeMappings[T1] | ImplementedKindToNodeMappings[T2])[] {
  return [...(grouped[k1] ?? []), ...(grouped[k2] ?? [])];
}

export function groupMembers(members: Iterable<Node>): {
  methods: Record<string, Signature[]>;
  properties: (PropertySignature | PropertyDeclaration)[];
  constructors: (ConstructSignatureDeclaration | ConstructorDeclaration)[];
} {
  const grouped = groupBySyntaxKind(members);
  const allProperties = getGroupedTypes(
    grouped,
    SyntaxKind.PropertySignature,
    SyntaxKind.PropertyDeclaration,
  );
  function isMethod(prop: PropertySignature | PropertyDeclaration) {
    if (prop.hasQuestionToken()) {
      // We have to treat all optional methods as properties instead:
      // f : () => void   translates to   def f(self) -> None: ...
      // f?: () => void   translates to   f: Callable[[], None] | None = ...
      return false;
    }
    return prop.getTypeNode()?.isKind(SyntaxKind.FunctionType);
  }

  const { functions = [], properties = [] } = groupBy(allProperties, (prop) =>
    isMethod(prop) ? "functions" : "properties",
  );
  const methodSigs = getGroupedTypes(
    grouped,
    SyntaxKind.MethodSignature,
    SyntaxKind.MethodDeclaration,
  );
  const empty: [
    string,
    FunctionTypeNode | MethodSignature | MethodDeclaration,
  ][] = [];
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
  const constructors = getGroupedTypes(
    grouped,
    SyntaxKind.ConstructSignature,
    SyntaxKind.Constructor,
  );
  return { methods, properties, constructors };
}

function resolveTypeParameterDefault(
  decl: TypeParameterDeclaration,
  paramDecls: TypeParameterDeclaration[],
  resolvedTypeArgs: TypeNode[],
): TypeNode {
  const defaultType = decl.getDefaultOrThrow();

  // Hacky: do text replacement on the type text.
  // There must be a better way?
  let typeText = defaultType.getText();
  let hasSubstitution = false;

  // Replace any type parameter names with their resolved types. Probably we
  // should only apply substitutions up the position of the current type
  // parameter. We'll see if it matters.
  for (
    let i = 0;
    i < Math.min(paramDecls.length, resolvedTypeArgs.length);
    i++
  ) {
    const paramName = paramDecls[i].getName();
    const resolvedText = resolvedTypeArgs[i].getText();

    const regex = new RegExp(String.raw`\b${paramName}\b`, "g");
    const newTypeText = typeText.replace(regex, resolvedText);
    if (newTypeText !== typeText) {
      typeText = newTypeText;
      hasSubstitution = true;
    }
  }

  if (!hasSubstitution) {
    return defaultType;
  }

  // Create a temporary type alias to parse the substituted type
  const project = decl.getSourceFile().getProject();
  const tempFileName = `temp_${Date.now()}_${Math.random().toString(36).slice(2, 9)}.ts`;
  const tempSource = project.createSourceFile(
    tempFileName,
    `type Temp = ${typeText};`,
  );
  const tempAlias = tempSource.getTypeAliases()[0];
  const substitutedType = tempAlias.getTypeNode()!;

  return substitutedType;
}

export function getExpressionTypeArgs(
  ident: EntityName,
  expression: TypeArgumentedNode & Node,
): TypeNode[] {
  if (Node.isQualifiedName(ident)) {
    ident = ident.getRight();
  }
  const typeArgNodes = expression.getTypeArguments();
  // Get the declarations to find the expected number of type parameters
  const seenNames: string[] = [];
  const paramDecls: TypeParameterDeclaration[] = [];
  const defs = ident
    .getDefinitionNodes()
    .filter(
      (
        node,
      ): node is
        | InterfaceDeclaration
        | TypeAliasDeclaration
        | ClassDeclaration =>
        Node.isInterfaceDeclaration(node) ||
        Node.isTypeAliasDeclaration(node) ||
        Node.isClassDeclaration(node),
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

  // If we have fewer explicit type arguments than parameters, fill in defaults
  if (typeArgNodes.length < paramDecls.length) {
    const missingDecls = paramDecls.slice(typeArgNodes.length);
    for (const decl of missingDecls) {
      const defaultType = resolveTypeParameterDefault(
        decl,
        paramDecls,
        typeArgNodes,
      );
      typeArgNodes.push(defaultType);
    }
  }
  return typeArgNodes;
}

export function classifyIdentifier(ident: Identifier): ClassifiedIdentifier {
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
    logger.info(
      name,
      rest.map((x) => x.getKindName()),
    );
    const decl = rest.filter(Node.isVariableDeclaration)?.[0];
    if (decl) {
      return {
        kind: "varDecl",
        name,
        decl,
        ifaces,
      };
    }
    logger.error("Failed to classify", name, "locations:");
    for (const decl of rest) {
      logger.error(getNodeLocation(decl));
    }
    throw new Error(`Unclear how to classify identifier ${name}`);
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

export function isValidPythonIdentifier(name: string): boolean {
  return /^[a-zA-Z_$][a-zA-Z0-9_]*$/.test(name);
}

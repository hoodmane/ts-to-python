import { ConstructSignatureDeclaration, EntityName, FunctionTypeNode, Identifier, InterfaceDeclaration, MethodSignature, Node, PropertySignature, Signature, SyntaxKind, TypeAliasDeclaration, TypeArgumentedNode, TypeElementTypes, TypeNode, TypeParameterDeclaration } from "ts-morph";
import { WrappedGen, groupBy, groupByGen, split } from "./groupBy";
import { ClassifiedIdentifier, GroupedBySyntaxKind } from "./types";

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

export function groupMembers(members: TypeElementTypes[]): {
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

export function getExpressionTypeArgs(
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
    console.log(name, rest.map(x => x.getKindName()));
    const decl = rest.filter(Node.isVariableDeclaration)?.[0];
    if (decl) {
      return {
        kind: "varDecl",
        name,
        decl,
        ifaces,
      };
    }
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
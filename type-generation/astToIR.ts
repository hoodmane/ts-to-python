import {
  EntityName,
  InterfaceDeclaration,
  IntersectionTypeNode,
  LiteralTypeNode,
  Node,
  PropertySignature,
  Signature,
  SignaturedDeclaration,
  SyntaxKind,
  TypeElementTypes,
  TypeNode,
  TypeOperatorTypeNode,
  TypeReferenceNode,
  UnionTypeNode,
} from "ts-morph";
import { TYPE_TEXT_MAP } from "./adjustments";
import { split } from "./groupBy";
import {
  getExpressionTypeArgs,
  getNodeLocation,
  groupMembers,
} from "./astUtils";
import { uniqBy } from "./render";

export type TypeIR =
  | SimpleTypeIR
  | UnionTypeIR
  | IntersectionTypeIR
  | ParenTypeIR
  | ParameterReferenceTypeIR
  | ReferenceTypeIR
  | OtherTypeIR
  | TypeOperatorTypeIR
  | TupleTypeIR
  | ArrayTypeIR
  | CallableIR;

type SimpleTypeIR = { kind: "simple"; text: string };
type UnionTypeIR = { kind: "union"; types: TypeIR[] };
type IntersectionTypeIR = {
  kind: "intersection";
  types: TypeIR[];
  location: string;
};
type TupleTypeIR = { kind: "tuple"; types: TypeIR[] };
type ArrayTypeIR = { kind: "array"; type: TypeIR };
type ParenTypeIR = { kind: "paren"; type: TypeIR };
type TypeOperatorTypeIR = {
  kind: "operator";
  operatorName: string;
  type: TypeIR;
};
type OtherTypeIR = { kind: "other"; nodeKind: string; location: string };

type ParameterReferenceTypeIR = { kind: "parameterReference"; name: string };
export type ReferenceTypeIR = {
  kind: "reference";
  identName: string;
  // EntityName is not structuredCloneable, and we don't really want to clone it
  // in any case. So we stick the EntityName in a list and store the index.
  identIndex: number;
  typeArgs: TypeIR[];
};

export type ParamIR = {
  name: string;
  type: TypeIR;
  optional: boolean;
};
export type SigIR = {
  params: ParamIR[];
  spreadParam?: ParamIR;
  kwparams?: ParamIR[];
  returns: TypeIR;
};

export type SigGroupIR = {
  name: string;
  sigs: SigIR[];
  isStatic?: boolean;
};

type CallableIR = {
  kind: "callable";
  signatures: SigIR[];
};

export type PropertyIR = {
  type: TypeIR;
  name: string;
  isOptional: boolean;
  isStatic: boolean;
  isReadonly: boolean;
};

export type InterfaceIR = {
  name: string;
  methods: SigGroupIR[];
  properties: PropertyIR[];
  typeParams: string[];
};

function simpleType(text: string): SimpleTypeIR {
  return { kind: "simple", text };
}

function unionType(types: TypeIR[]): UnionTypeIR {
  return { kind: "union", types };
}

function intersectionType(
  types: TypeIR[],
  location: string,
): IntersectionTypeIR {
  return { kind: "intersection", types, location };
}

function tupleType(types: TypeIR[]): TupleTypeIR {
  return { kind: "tuple", types };
}

function arrayType(type: TypeIR): ArrayTypeIR {
  return { kind: "array", type };
}

function parenType(type: TypeIR): ParenTypeIR {
  return { kind: "paren", type };
}

const ANY_IR = simpleType("Any");

function typeLiteralToIR(typeNode: LiteralTypeNode): TypeIR {
  let text = typeNode.getText();
  if (text === "null") {
    return simpleType("None");
  }
  if (text === "true") {
    text = "True";
  }
  if (text === "false") {
    text = "False";
  }
  return simpleType(`Literal[${text}]`);
}

const operatorToName = {
  [SyntaxKind.ReadonlyKeyword]: "readonly",
  [SyntaxKind.UniqueKeyword]: "unique",
};

function typeOperatorToIR(typeNode: TypeOperatorTypeNode): TypeOperatorTypeIR {
  const operator = typeNode.getOperator();
  const operatorName = operatorToName[operator];
  if (!operatorName) {
    throw new Error("Unknown type operator " + operator);
  }
  const type = typeToIR(typeNode.getTypeNode());
  return { kind: "operator", operatorName, type };
}

function unionToIR(typeNode: UnionTypeNode, isOptional: boolean): TypeIR {
  const unionTypes = typeNode.getTypeNodes() as TypeNode[];
  const [literals, rest] = split<TypeNode, LiteralTypeNode>(
    unionTypes,
    Node.isLiteralTypeNode,
  );
  const types = rest.map((ty) => typeToIR(ty, false));
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
    types.push(simpleType(`Literal[${lits.join(", ")}]`));
  }
  if (isOptional) {
    types.push(simpleType("None"));
  }
  if (types.length === 1) {
    return types[0];
  }
  return unionType(types);
}

function intersectionToIR(typeNode: IntersectionTypeNode) {
  const filteredTypes = typeNode
    .getTypeNodes()
    .filter(
      (type) =>
        !(Node.isThisTypeNode(type) || type.getText().startsWith("ThisType<")),
    );
  if (filteredTypes.length === 1) {
    return typeToIR(filteredTypes[0]);
  }
  const typeString = typeNode.getType().getText();
  if (typeString === "Window & typeof globalThis") {
    return ANY_IR;
  }
  if (typeString === "ArrayBufferLike & { BYTES_PER_ELEMENT?: never; }") {
    return simpleType("ArrayBuffer");
  }
  return intersectionType(
    filteredTypes.map((ty) => typeToIR(ty)),
    getNodeLocation(typeNode),
  );
}

// EntityName is not structuredCloneable, and we don't really want to clone it
// in any case. So we stick the EntityName in this list and store the index.
export const IDENT_ARRAY: EntityName[] = [];
function typeReferenceToIR(typeNode: TypeReferenceNode): TypeIR {
  if (Node.isTypeReference(typeNode)) {
    const ident = typeNode.getTypeName();
    if (typeNode.getType().isTypeParameter()) {
      const name = ident.getText();
      return { kind: "parameterReference", name };
    }
    const typeArgs = getExpressionTypeArgs(ident, typeNode).map((ty) =>
      typeToIR(ty),
    );
    const identName = ident.getText();
    const identIndex = IDENT_ARRAY.push(ident) - 1;
    return { kind: "reference", identName, identIndex, typeArgs };
  }
}

function otherTypeToIR(node: Node): OtherTypeIR {
  const nodeKind = node.getKindName();
  const location = getNodeLocation(node);
  return { kind: "other", nodeKind, location };
}

export function typeToIR(
  typeNode: TypeReferenceNode,
  isOptional?: boolean,
): ReferenceTypeIR | ParameterReferenceTypeIR;
export function typeToIR(typeNode: TypeNode, isOptional?: boolean): TypeIR;
export function typeToIR(
  typeNode: TypeNode,
  isOptional: boolean = false,
): TypeIR {
  const typeText = typeNode.getText();
  if (typeText === "number") {
    return unionType([simpleType("int"), simpleType("float")]);
  }
  if (typeText in TYPE_TEXT_MAP) {
    return simpleType(TYPE_TEXT_MAP[typeText]);
  }
  if (Node.isFunctionTypeNode(typeNode)) {
    const signatures = typeNode.getType().getCallSignatures().map(sigToIR);
    return { kind: "callable", signatures };
  }
  if (Node.isUnionTypeNode(typeNode)) {
    return unionToIR(typeNode, isOptional);
  }
  if (Node.isParenthesizedTypeNode(typeNode)) {
    return parenType(typeToIR(typeNode.getTypeNode(), false));
  }
  if (Node.isThisTypeNode(typeNode)) {
    return simpleType("Self");
  }
  if (Node.isLiteralTypeNode(typeNode)) {
    return typeLiteralToIR(typeNode);
  }
  if (Node.isIntersectionTypeNode(typeNode)) {
    return intersectionToIR(typeNode);
  }
  if (Node.isTypeOperatorTypeNode(typeNode)) {
    return typeOperatorToIR(typeNode);
  }
  if (Node.isTypeReference(typeNode)) {
    return typeReferenceToIR(typeNode);
  }
  if (Node.isTemplateLiteralTypeNode(typeNode)) {
    return simpleType("str");
  }
  if (Node.isArrayTypeNode(typeNode)) {
    const eltType = typeToIR(typeNode.getElementTypeNode());
    return arrayType(eltType);
  }
  if (Node.isTupleTypeNode(typeNode)) {
    const elts = typeNode.getElements().map((elt) => typeToIR(elt));
    return tupleType(elts);
  }
  if (Node.isTypePredicate(typeNode)) {
    return simpleType("bool");
  }
  const signatures = typeNode.getType().getCallSignatures();
  if (signatures.length > 0) {
    throw new Error("oops");
  }
  return otherTypeToIR(typeNode);
}

export function sigToIR(sig: Signature): SigIR {
  const decl = sig.getDeclaration() as SignaturedDeclaration;
  try {
    const pyParams: ParamIR[] = [];
    let spreadParam: ParamIR;
    for (const param of decl.getParameters()) {
      const spread = !!param.getDotDotDotToken();
      const optional = !!param.hasQuestionToken();
      const type = typeToIR(param.getTypeNode()!, optional);
      const pyParam: ParamIR = { name: param.getName(), type, optional };
      if (spread) {
        if (type.kind !== "array") {
          throw new Error("expected type array for spread param");
        }
        pyParam.type = type.type;
        spreadParam = pyParam;
        continue;
      }
      pyParams.push(pyParam);
    }
    const retNode = decl.getReturnTypeNode()!;
    const returns = typeToIR(retNode);
    return { params: pyParams, spreadParam, returns };
  } catch (e) {
    console.warn("failed to convert", sig.getDeclaration().getText());
    throw e;
  }
}

/**
 * Helper for sigToIRDestructure
 *
 * If the parameter of sig is an Interface, from Python we allow the interface
 * entries to be passed as keyword arguments. Return the InterfaceDeclaration if
 * the last parameter is an interface, else return undefined.
 *
 * TODO: make this work for type literals too?
 */
function getInterfaceDeclToDestructure(
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

/**
 * If the last parameter of sig is an interface, return a pair of signatures:
 * 1. the original signature unaltered
 * 2. a signature where the entries of the last parameter are passed as key word
 *    arguments.
 *
 * TODO: is this correct when all entries of the interface are optional?
 * TODO: What about if the last argument is an object type literal?
 */
export function sigToIRDestructure(sig: Signature): [SigIR] | [SigIR, SigIR] {
  const sigIR = sigToIR(sig);
  const toDestructure = getInterfaceDeclToDestructure(sig);
  if (!toDestructure) {
    return [sigIR];
  }
  const sigIRDestructured = structuredClone(sigIR);
  sigIRDestructured.params.pop();
  const kwargs: ParamIR[] = [];
  for (const prop of toDestructure.getProperties()) {
    const name = prop.getName();
    const optional = !!prop.getQuestionTokenNode();
    const type = typeToIR(prop.getTypeNode()!, optional);
    kwargs.push({ name, type, optional });
  }
  sigIRDestructured.kwparams = kwargs;
  return [sigIR, sigIRDestructured];
}

function overloadGroupToIR(
  name: string,
  signatures: Signature[],
  isStatic?: boolean,
): SigGroupIR {
  const sigs = signatures.flatMap((sig) => sigToIRDestructure(sig));
  return { name, sigs, isStatic };
}

export function propertySignatureToIR(
  member: PropertySignature,
  isStatic: boolean = false,
): PropertyIR {
  const name = member.getName();
  const isOptional = member.hasQuestionToken();
  const type = typeToIR(member.getTypeNode()!, isOptional);
  const isReadonly = member.isReadonly();
  return { name, type, isOptional, isStatic, isReadonly };
}

export function interfaceToIR(
  name: string,
  members: TypeElementTypes[],
  staticMembers: TypeElementTypes[],
  typeParams: string[],
): InterfaceIR {
  const { methods: astMethods, properties: astProperties } =
    groupMembers(members);
  const {
    methods: staticAstMethods,
    properties: staticAstProperties,
    constructors,
  } = groupMembers(staticMembers);
  const propMap = new Map(astProperties.map((prop) => [prop.getName(), prop]));
  for (const key of Object.keys(staticAstMethods)) {
    delete astMethods[key];
  }
  typeParams = Array.from(new Set(typeParams));
  const extraMethods: SigGroupIR[] = [];
  if ("[Symbol.iterator]" in astMethods) {
    const x = astMethods["[Symbol.iterator]"];
    delete astMethods["[Symbol.iterator]"];
    const typeNode = x[0]
      .getDeclaration()
      .getReturnTypeNode()
      .asKindOrThrow(SyntaxKind.TypeReference);
    if (typeNode.getTypeName().getText() !== "IterableIterator") {
      throw new Error("Surprise!");
    }
    const returns = typeToIR(typeNode);
    if (returns.kind === "parameterReference") {
      throw new Error("Cannot happen!");
    }
    returns.identIndex = -1;
    returns.identName = "PyIterator";
    extraMethods.push({ name: "__iter__", sigs: [{ params: [], returns }] });
  }
  if (
    name !== "Function_iface" &&
    (propMap.get("size")?.getTypeNode().getText() === "number" ||
      propMap.get("length")?.getTypeNode().getText() === "number")
  ) {
    extraMethods.push({
      name: "__len__",
      sigs: [{ params: [], returns: simpleType("int") }],
    });
  }
  const redirectMethod = (origName: string, newName: string): void => {
    if (!(origName in astMethods)) {
      return;
    }
    extraMethods.push(overloadGroupToIR(newName, astMethods[origName]));
  };
  redirectMethod("has", "__contains__");
  redirectMethod("includes", "__contains__");
  redirectMethod("get", "__getitem__");
  redirectMethod("set", "__setitem__");
  redirectMethod("delete", "__delitem__");
  if (constructors) {
    staticAstMethods["new"] = constructors.map((decl) => decl.getSignature());
  }
  const irMethods = ([] as SigGroupIR[]).concat(
    Object.entries(astMethods).map(([name, sigs]) =>
      overloadGroupToIR(name, sigs, false),
    ),
    Object.entries(staticAstMethods).map(([name, sigs]) =>
      overloadGroupToIR(name, sigs, true),
    ),
    extraMethods,
  );
  const irProps = ([] as PropertyIR[]).concat(
    astProperties.map((prop) => propertySignatureToIR(prop, false)),
    staticAstProperties.map((prop) => propertySignatureToIR(prop, true)),
  );
  const props = uniqBy(irProps, ({ name }) => name);
  return { methods: irMethods, properties: props, name, typeParams };
}
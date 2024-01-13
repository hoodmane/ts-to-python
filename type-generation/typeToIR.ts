import {
  EntityName,
  IntersectionTypeNode,
  LiteralTypeNode,
  Node,
  Signature,
  SignaturedDeclaration,
  SyntaxKind,
  TypeNode,
  TypeOperatorTypeNode,
  TypeReferenceNode,
  UnionTypeNode,
} from "ts-morph";
import { TYPE_TEXT_MAP } from "./adjustments";
import { split } from "./groupBy";
import { Variance } from "./types";
import { getExpressionTypeArgs, getNodeLocation } from "./astUtils";

type TypeIR =
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
type IntersectionTypeIR = { kind: "intersection"; types: TypeIR[] };
type TupleTypeIR = { kind: "tuple"; types: TypeIR[] };
type ArrayTypeIR = { kind: "array"; type: TypeIR };
type ParenTypeIR = { kind: "paren"; type: TypeIR };
type TypeOperatorTypeIR = { kind: "operator"; operatorName: string; type: TypeIR; };
type OtherTypeIR = { kind: "other"; nodeKind: string; location: string };

type ParameterReferenceTypeIR = { kind: "parameterReference"; name: string };
export type ReferenceTypeIR = {
  kind: "reference";
  ident: EntityName;
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

type CallableIR = {
  kind: "callable";
  signatures: SigIR[];
};

function simpleType(text: string): SimpleTypeIR {
  return { kind: "simple", text };
}

function unionType(types: TypeIR[]): UnionTypeIR {
  return { kind: "union", types };
}

function intersectionType(types: TypeIR[]): IntersectionTypeIR {
  return { kind: "intersection", types };
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
  return intersectionType(filteredTypes.map((ty) => typeToIR(ty)));
}

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
    return { kind: "reference", ident, typeArgs };
  }
}

function otherTypeToIR(node: Node): OtherTypeIR {
  const nodeKind = node.getKindName();
  const location = getNodeLocation(node);
  return { kind: "other", nodeKind, location };
}

function sigToIR(sig: Signature): SigIR {
  const decl = sig.getDeclaration() as SignaturedDeclaration;
  try {
    const pyParams: ParamIR[] = [];
    let spreadParam: ParamIR;
    for (const param of decl.getParameters()) {
      const spread = !!param.getDotDotDotToken();
      const optional = !!param.hasQuestionToken();
      const type = typeToIR(
        param.getTypeNode()!,
        optional,
      );
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
  const callSignatures = typeNode.getType().getCallSignatures();
  if (callSignatures.length > 0) {
    if (!Node.isFunctionTypeNode(typeNode)) {
      console.warn("callable kind:", typeNode.getKindName());
      throw new Error("oops");
    }
    const signatures = callSignatures.map(sigToIR);
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
  return otherTypeToIR(typeNode);
}

import {
  CallSignatureDeclaration,
  ClassDeclaration,
  EntityName,
  FunctionDeclaration,
  Identifier,
  InterfaceDeclaration,
  IntersectionTypeNode,
  LiteralTypeNode,
  Node,
  PropertyDeclaration,
  PropertySignature,
  Signature,
  SignaturedDeclaration,
  SourceFile,
  SyntaxKind,
  ts,
  TypeElementTypes,
  TypeLiteralNode,
  TypeNode,
  TypeOperatorTypeNode,
  TypeParameterDeclaration,
  TypeParameteredNode,
  TypeReferenceNode,
  UnionTypeNode,
  VariableDeclaration,
} from "ts-morph";
import { BUILTIN_NAMES, TYPE_TEXT_MAP } from "./adjustments";
import { groupBy, popElt, split, split2 } from "./groupBy";
import {
  assertUnreachable,
  classifyIdentifier,
  getExpressionTypeArgs,
  getNodeLocation,
  groupMembers,
} from "./astUtils";
import { sanitizeReservedWords, uniqBy } from "./irToString";
import { Needed } from "./types";

export type TypeIR =
  | SimpleTypeIR
  | UnionTypeIR
  | ParenTypeIR
  | ParameterReferenceTypeIR
  | ReferenceTypeIR
  | OtherTypeIR
  | TypeOperatorTypeIR
  | TupleTypeIR
  | ArrayTypeIR
  | CallableIR
  | NumberTypeIR;

type SimpleTypeIR = { kind: "simple"; text: string };
type NumberTypeIR = { kind: "number" };
type UnionTypeIR = { kind: "union"; types: TypeIR[] };
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
  name: string;
  typeArgs: TypeIR[];
};

export type ParamIR = {
  name: string;
  type: TypeIR;
  isOptional: boolean;
};
export type SigIR = {
  params: ParamIR[];
  spreadParam?: ParamIR;
  kwparams?: ParamIR[];
  returns: TypeIR;
  typeParams?: string[];
};

/**
 * Ad hoc depth 2 copy
 *
 * It would be nice to use structuredClone but we'd like to store refs to the
 * EntityName and these are not cloneable.
 */
function depth2CopySig({
  params,
  spreadParam,
  kwparams,
  returns,
  typeParams,
}: SigIR): SigIR {
  params = Array.from(params);
  kwparams = kwparams && Array.from(kwparams);
  return { params, spreadParam, kwparams, returns, typeParams };
}

export type CallableIR = {
  kind: "callable";
  name?: string;
  signatures: SigIR[];
  isStatic?: boolean;
};

export type PropertyIR = {
  type: TypeIR;
  name: string;
  isOptional: boolean;
  isStatic: boolean;
  isReadonly: boolean;
};

export type InterfaceIR = {
  kind: "interface";
  name: string;
  methods: CallableIR[];
  properties: PropertyIR[];
  typeParams: string[];
  bases: BaseIR[];
  // Synthetic bases adjusted into the class.
  extraBases?: string[];
  // This is used to decide whether the class should be a Protocol or not.
  concrete?: boolean;
  // Control how numbers are rendered in the class. Normally they are rendered
  // as int | float, but sometimes we just want it to be written as int.
  // This is handled in an adhoc manner in adjustInterfaceIR.
  numberType?: string;
};

export type BaseIR = {
  name: string;
  typeArgs: TypeIR[];
};

export type DeclarationIR = {
  kind: "declaration";
  name: string;
  type: TypeIR;
};

export type TypeAliasIR = {
  kind: "typeAlias";
  name: string;
  type: TypeIR;
  typeParams: string[];
};

export type TopLevelIR = DeclarationIR | InterfaceIR | TypeAliasIR | CallableIR;

export function simpleType(text: string): SimpleTypeIR {
  return { kind: "simple", text };
}

function unionType(types: TypeIR[]): UnionTypeIR {
  return { kind: "union", types };
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

function declarationIR(name: string, type: TypeIR): DeclarationIR {
  return { kind: "declaration", name, type };
}

const ANY_IR = simpleType("Any");

function literalType(text: string): TypeIR {
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

function literalTypeToIR(typeNode: LiteralTypeNode): TypeIR {
  return literalType(typeNode.getText());
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
): InterfaceDeclaration | TypeLiteralNode | undefined {
  const decl = sig.getDeclaration() as SignaturedDeclaration;
  const typeNode = decl.getParameters().at(-1)?.getTypeNode();
  if (!typeNode) {
    return undefined;
  }

  // Handle inline object types (TypeLiteral)
  const typeLiteral = typeNode.asKind(SyntaxKind.TypeLiteral);
  if (typeLiteral) {
    return typeLiteral;
  }

  // Handle named interface types (TypeReference)
  const defs = typeNode
    .asKind(SyntaxKind.TypeReference)
    ?.getTypeName()
    ?.asKind(SyntaxKind.Identifier)
    ?.getDefinitionNodes();
  if (!defs?.length) {
    return undefined;
  }
  return defs[0].asKind(SyntaxKind.InterfaceDeclaration);
}

function getFilteredTypeParams<T extends TypeParameteredNode>(
  t: T,
): TypeParameterDeclaration[] {
  return t
    .getTypeParameters()
    .filter((p) => p.getConstraint()?.getText() !== "string");
}

function getLiteralTypeArgSet(node: TypeNode): Set<string> {
  const ty = node.getType();
  if (ty.isLiteral()) {
    return new Set([ty.getLiteralValueOrThrow()]) as Set<string>;
  } else if (ty.isUnion()) {
    return new Set(
      ty.getUnionTypes().map((x) => x.getLiteralValueOrThrow()),
    ) as Set<string>;
  }
  throw new Error("Unexpected type");
}

/**
 * Helper for getting the bases in membersDeclarationToIR
 */
function getInterfaceTypeArgs(ident: EntityName): TypeIR[] {
  return uniqBy(
    (ident as Identifier)
      .getDefinitionNodes()
      .filter(Node.isInterfaceDeclaration)
      .flatMap(getFilteredTypeParams)
      .map((param) => param.getName()),
    (param) => param,
  ).map((name) => ({ kind: "parameterReference", name }));
}

const operatorToName: {
  [K in ts.TypeOperatorNode["operator"]]: string;
} = {
  [SyntaxKind.ReadonlyKeyword]: "readonly",
  [SyntaxKind.UniqueKeyword]: "unique",
  [SyntaxKind.KeyOfKeyword]: "keyof",
};

type SyntheticTypeRoot =
  | {
      kind: "intersection";
      node: IntersectionTypeNode;
    }
  | {
      kind: "typeLiteral";
      node: TypeLiteralNode;
    }
  | {
      kind: "omit";
      node: TypeReferenceNode;
    }
  | {
      kind: "pick";
      node: TypeReferenceNode;
    }
  | {
      kind: "partial";
      node: TypeReferenceNode;
    };

function classifySyntheticType(node: TypeNode): SyntheticTypeRoot | undefined {
  if (Node.isIntersectionTypeNode(node)) {
    return { kind: "intersection", node };
  }
  if (Node.isTypeLiteral(node)) {
    return { kind: "typeLiteral", node };
  }
  if (!Node.isTypeReference(node)) {
    return undefined;
  }
  const ident = node.getTypeName() as Identifier;
  const name = ident.getText();
  if (name === "Omit") {
    return { kind: "omit", node };
  }
  if (name === "Partial") {
    return { kind: "partial", node };
  }
  if (name === "Pick") {
    return { kind: "pick", node };
  }
  return undefined;
}

type Modifier = {
  partial: boolean;
  omitSet?: Set<string>;
  pickSet?: Set<string>;
};

class SyntheticTypeConverter {
  // Handling for converting types that have to create generated classes
  // Currently it has no state (other than recording that nameContext is not
  // undefined) and is just factored away from converter to keep things tidier.
  converter: Converter;
  constructor(converter: Converter) {
    this.converter = converter;
  }
  get nameContext(): string[] {
    const ctx = this.converter.nameContext;
    if (!ctx) {
      throw new Error("Should not happen");
    }
    return ctx;
  }

  hasWork(base: TypeNode, modifiers: Modifier) {
    const { partial, omitSet, pickSet } = modifiers;
    if (partial) {
      return true;
    }
    if (omitSet) {
      for (const prop of base.getType().getProperties()) {
        if (omitSet.has(prop.getName())) {
          return true;
        }
      }
    }
    if (pickSet) {
      for (const prop of base.getType().getProperties()) {
        if (!pickSet.has(prop.getName())) {
          return true;
        }
      }
    }
    return false;
  }

  doConversion(
    nodes: Pick<TypeLiteralNode, "getMembers">[],
    modifiers: Modifier,
  ): ReferenceTypeIR {
    let name = this.nameContext.join("__");
    if (!name.endsWith("_iface")) {
      name += "_iface";
    }
    let members = nodes.flatMap((x) => x.getMembers());
    const { omitSet, partial, pickSet } = modifiers;
    if (omitSet) {
      members = members.filter(
        (x) => !Node.isPropertyNamed(x) || !omitSet.has(x.getName()),
      );
    }
    if (pickSet) {
      members = members.filter(
        (x) => !Node.isPropertyNamed(x) || pickSet.has(x.getName()),
      );
    }
    const result = this.converter.interfaceToIR(name, [], members, [], [], []);
    if (partial) {
      for (const prop of result.properties) {
        prop.isOptional = true;
      }
    }
    this.converter.extraTopLevels.push(result);
    return { kind: "reference", name, typeArgs: [] };
  }

  classifiedTypeToIr(
    typeRoot: SyntheticTypeRoot,
    modifiersArg?: Modifier,
  ): TypeIR {
    let modifiers = modifiersArg ?? { partial: false };
    switch (typeRoot.kind) {
      case "intersection": {
        const node = typeRoot.node;
        const types = node
          .getTypeNodes()
          .map((ty, idx) => {
            this.nameContext.push(`Intersection${idx}`);
            // In TS it's just fine to use the LHS of a type alias in an
            // intersection. However, in Python we can't inherit from a type
            // alias. So if the reference target is a type alias, we have to
            // unwind it until we hit something we can inherit.
            while (Node.isTypeReference(ty)) {
              const classified = classifyIdentifier(ty.getTypeName() as Identifier);
              if (classified.kind !== "typeAlias") {
                break;
              }
              ty = classified.decl.getTypeNode()!;
            }
            const res = this.typeToIR(ty, modifiers);
            this.nameContext.pop();
            return res;
          })
          .filter((x): x is ReferenceTypeIR => !!x && x.kind === "reference");
        const name = this.nameContext.join("__") + "_iface";
        this.converter.extraTopLevels.push(
          this.converter.interfaceToIR(name, types, [], [], [], []),
        );
        return { kind: "reference", name, typeArgs: [] };
      }
      case "typeLiteral": {
        return this.doConversion([typeRoot.node], modifiers);
      }
      case "omit": {
        const node = typeRoot.node;
        const base = node.getTypeArguments()[0]!;
        const toOmitType = node.getTypeArguments()[1]!;
        modifiers = structuredClone(modifiers);
        modifiers.omitSet = getLiteralTypeArgSet(toOmitType);
        this.nameContext.push("Omit");
        const result = this.typeToIR(base, modifiers);
        this.nameContext.pop();
        return result;
      }
      case "pick": {
        const node = typeRoot.node;
        const base = node.getTypeArguments()[0]!;
        const toPickType = node.getTypeArguments()[1]!;
        modifiers = structuredClone(modifiers);
        modifiers.pickSet = getLiteralTypeArgSet(toPickType);
        this.nameContext.push("Pick");
        const res = this.typeToIR(base, modifiers);
        this.nameContext.pop();
        return res;
      }
      case "partial": {
        const node = typeRoot.node;
        const base = node.getTypeArguments()[0]!;
        modifiers = structuredClone(modifiers);
        modifiers.partial = true;
        this.nameContext.push("Partial");
        const result = this.typeToIR(base, modifiers);
        this.nameContext.pop();
        return result;
      }
    }
  }

  typeToIR(n: TypeNode, modifiers: Modifier): TypeIR {
    const typeRoot = classifySyntheticType(n);
    if (typeRoot) {
      return this.classifiedTypeToIr(typeRoot, modifiers);
    }
    if (!this.hasWork(n, modifiers)) {
      return this.converter.typeToIR(n);
    }
    if (Node.isTypeReference(n)) {
      const res = classifyIdentifier(n.getTypeName() as Identifier);
      if (res.kind === "typeAlias") {
        return this.typeToIR(res.decl.getTypeNode()!, modifiers);
      }
      if (res.kind === "interfaces") {
        const { name, ifaces } = res;
        const typeParams = this.converter.getTypeParamsFromDecls(ifaces);
        if (typeParams.length > 0) {
          throw new Error("Not handled");
        }
        this.nameContext.push(name);
        const result = this.doConversion(ifaces, modifiers);
        this.nameContext.pop();
        return result;
      }
      throw new Error(`Not handled ${res.kind}`);
    }
    throw new Error(`Not handled ${n.getKindName()}`);
  }
}

export class Converter {
  ifaceTypeParamConstraints: Map<string, string>;
  classTypeParams: Set<string>;
  neededSet: Set<Needed>;
  convertedSet: Set<string>;
  extraTopLevels: TopLevelIR[];
  nameContext: string[] | undefined;

  constructor() {
    this.ifaceTypeParamConstraints = new Map();
    this.classTypeParams = new Set();
    this.neededSet = new Set();
    this.convertedSet = new Set(BUILTIN_NAMES);
    this.extraTopLevels = [];
    this.nameContext = undefined;
  }

  pushNameContext(ctx: string): void {
    if (this.nameContext) {
      this.nameContext.push(ctx);
    }
  }

  popNameContext() {
    if (this.nameContext) {
      this.nameContext.pop();
    }
  }

  addNeededIdentifier(ident: Identifier): void {
    if (Node.isQualifiedName(ident)) {
      throw new Error("Qualified name! " + ident.getText());
    }
    this.neededSet.add({ type: "ident", ident });
  }

  addNeededInterface(ident: Identifier): void {
    this.neededSet.add({ type: "interface", ident });
  }

  setIfaceTypeConstraints<T extends TypeParameteredNode>(ifaces: T[]) {
    // Set type parameter constraints for interface type parameters
    for (const iface of ifaces) {
      for (const param of iface.getTypeParameters()) {
        const constraint = param.getConstraint();
        if (constraint) {
          this.ifaceTypeParamConstraints.set(
            param.getName(),
            constraint.getText(),
          );
        }
      }
    }
  }

  typeOperatorToIR(typeNode: TypeOperatorTypeNode): TypeOperatorTypeIR {
    const operator = typeNode.getOperator();
    const operatorName = operatorToName[operator];
    if (!operatorName) {
      throw new Error("Unknown type operator " + SyntaxKind[operator]);
    }
    const type = this.typeToIR(typeNode.getTypeNode());
    return { kind: "operator", operatorName, type };
  }

  unionToIR(typeNode: UnionTypeNode, isOptional: boolean): TypeIR {
    const unionTypes = typeNode.getTypeNodes() as TypeNode[];
    const [literals, rest] = split<TypeNode, LiteralTypeNode>(
      unionTypes,
      Node.isLiteralTypeNode,
    );
    const types = rest.map((ty, idx) => {
      this.pushNameContext(`Union${idx}`);
      const res = this.typeToIR(ty, false);
      this.popNameContext();
      return res;
    });
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

  typeReferenceToIR(typeNode: TypeReferenceNode): TypeIR {
    const ident = typeNode.getTypeName();
    if (typeNode.getType().isTypeParameter()) {
      const name = ident.getText();

      // If type parameter extends string, translate to str directly
      // This doesn't work correctly if a function type param shadows an interface type param.
      // We can fix that if we have to.
      const interfaceConstraint = this.ifaceTypeParamConstraints.get(name);
      if (interfaceConstraint === "string") {
        return simpleType("str");
      }

      return { kind: "parameterReference", name };
    }
    let typeArgs = getExpressionTypeArgs(ident, typeNode).map((ty) =>
      this.typeToIR(ty),
    );
    if (Node.isQualifiedName(ident)) {
      return ANY_IR;
    }
    const { name, kind } = classifyIdentifier(ident);

    if (kind === "interfaces") {
      const interfaceDefs = (ident as Identifier)
        .getDefinitionNodes()
        .filter(Node.isInterfaceDeclaration);
      if (interfaceDefs.length > 0) {
        const typeParams = interfaceDefs[0].getTypeParameters();
        typeArgs = typeArgs.filter((_, index) => {
          if (index < typeParams.length) {
            const param = typeParams[index];
            const constraint = param.getConstraint();
            // Filter out if this type parameter extends string
            return !(constraint && constraint.getText() === "string");
          }
          return true;
        });
      }
    }

    if (!this.convertedSet.has(name)) {
      if (kind === "interfaces") {
        this.addNeededInterface(ident);
      } else {
        this.addNeededIdentifier(ident);
      }
    }
    return { kind: "reference", name: name, typeArgs };
  }

  otherTypeToIR(node: Node): OtherTypeIR {
    const nodeKind = node.getKindName();
    const location = getNodeLocation(node);
    return { kind: "other", nodeKind, location };
  }

  maybeSyntheticTypeToIR(node: TypeNode): TypeIR | undefined {
    if (!this.nameContext) {
      return undefined;
    }
    const typeRoot = classifySyntheticType(node);
    if (!typeRoot) {
      return undefined;
    }
    return new SyntheticTypeConverter(this).classifiedTypeToIr(typeRoot);
  }

  typeToIR(
    typeNode: TypeReferenceNode,
    isOptional?: boolean,
  ): ReferenceTypeIR | ParameterReferenceTypeIR;
  typeToIR(typeNode: TypeNode, isOptional?: boolean): TypeIR;
  typeToIR(typeNode: TypeNode, isOptional: boolean = false): TypeIR {
    const synthResult = this.maybeSyntheticTypeToIR(typeNode);
    if (synthResult) {
      return synthResult;
    }
    const typeText = typeNode.getText();
    if (typeText === "number") {
      return { kind: "number" };
    }
    if (typeText in TYPE_TEXT_MAP) {
      return simpleType(TYPE_TEXT_MAP[typeText]);
    }
    if (Node.isFunctionTypeNode(typeNode)) {
      const sigs = typeNode
        .getType()
        .getCallSignatures()
        .map((sig) => this.sigToIR(sig));
      return { kind: "callable", signatures: sigs };
    }
    if (Node.isUnionTypeNode(typeNode)) {
      return this.unionToIR(typeNode, isOptional);
    }
    if (Node.isParenthesizedTypeNode(typeNode)) {
      return parenType(this.typeToIR(typeNode.getTypeNode(), false));
    }
    if (Node.isThisTypeNode(typeNode)) {
      return simpleType("Self");
    }
    if (Node.isLiteralTypeNode(typeNode)) {
      return literalTypeToIR(typeNode);
    }
    if (Node.isTypeLiteral(typeNode)) {
      // If there is a nameContext, this was handled by syntheticTypeToIr.
      return this.otherTypeToIR(typeNode);
    }
    if (Node.isTypeOperatorTypeNode(typeNode)) {
      return this.typeOperatorToIR(typeNode);
    }
    if (Node.isTypeReference(typeNode)) {
      return this.typeReferenceToIR(typeNode);
    }
    if (Node.isTemplateLiteralTypeNode(typeNode)) {
      return simpleType("str");
    }
    if (Node.isArrayTypeNode(typeNode)) {
      const eltType = this.typeToIR(typeNode.getElementTypeNode());
      return arrayType(eltType);
    }
    if (Node.isTupleTypeNode(typeNode)) {
      const elts = typeNode.getElements().map((elt) => {
        if (Node.isNamedTupleMember(elt)) {
          return this.typeToIR(elt.getTypeNode());
        }
        return this.typeToIR(elt);
      });
      return tupleType(elts);
    }
    if (Node.isTypePredicate(typeNode)) {
      return simpleType("bool");
    }
    const signatures = typeNode.getType().getCallSignatures();
    if (signatures.length > 0) {
      const location = getNodeLocation(typeNode);
      throw new Error("oops: " + location);
    }
    return this.otherTypeToIR(typeNode);
  }

  sigToIR(sig: Signature): SigIR {
    const decl1 = sig.getDeclaration();
    const decl =
      decl1.asKind(SyntaxKind.CallSignature) ??
      decl1.asKind(SyntaxKind.ConstructSignature) ??
      decl1.asKind(SyntaxKind.Constructor) ??
      decl1.asKind(SyntaxKind.FunctionDeclaration) ??
      decl1.asKind(SyntaxKind.FunctionType) ??
      decl1.asKind(SyntaxKind.MethodDeclaration) ??
      decl1.asKindOrThrow(SyntaxKind.MethodSignature);
    try {
      this.setIfaceTypeConstraints([decl]);

      const pyParams: ParamIR[] = [];
      let spreadParam: ParamIR | undefined;
      const params = decl.getParameters();
      for (let idx = 0; idx < params.length; idx++) {
        const param = params[idx];
        const spread = !!param.getDotDotDotToken();
        const optional = !!param.hasQuestionToken();
        const name = param.getName();
        const isValidPythonIdentifier = /^[a-zA-Z_$][a-zA-Z0-9_]*$/.test(name);
        const oldNameContext = this.nameContext?.slice();
        const paramType = param.getTypeNode()!;
        const isLast = idx === params.length - 1;
        if (isLast && Node.isTypeLiteral(paramType)) {
          // If it's the last argument and the type is a type literal, we'll
          // destructure it so don't make a type.
          this.nameContext = undefined;
        } else if (isValidPythonIdentifier) {
          this.pushNameContext(name);
        } else {
          this.nameContext = undefined;
        }
        const type = this.typeToIR(paramType, optional);
        this.nameContext = oldNameContext;
        const pyParam: ParamIR = {
          name,
          type,
          isOptional: optional,
        };
        if (spread) {
          spreadParam = pyParam;
          if (type.kind === "array") {
            pyParam.type = type.type;
          } else {
            console.warn(
              `expected type array for spread param, got ${type.kind}`,
            );
            pyParam.type = type;
          }
          continue;
        }
        pyParams.push(pyParam);
      }
      const retNode = decl.getReturnTypeNode();
      let returns: TypeIR;
      if (retNode) {
        returns = this.typeToIR(retNode);
      } else {
        // It was a constructor. The return type is determined by the parent
        // node.
        const classDecl = decl
          .getParent()
          .asKindOrThrow(SyntaxKind.ClassDeclaration);
        const name = classDecl.getName() + "_iface";
        const typeArgs: ParameterReferenceTypeIR[] = classDecl
          .getTypeParameters()
          .map((x) => ({ kind: "parameterReference", name: x.getName() }));
        returns = { kind: "reference", name, typeArgs };
      }

      // Extract type parameters for this specific signature
      const typeParams = this.getTypeParamsFromDecl(decl);
      const result: SigIR = { params: pyParams, spreadParam, returns };
      if (typeParams.length > 0) {
        result.typeParams = typeParams;
      }
      return result;
    } catch (e) {
      console.warn("failed to convert", sig.getDeclaration().getText());
      console.warn(getNodeLocation(sig.getDeclaration()));
      throw e;
    }
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
  sigToIRDestructure(sig: Signature): [SigIR] | [SigIR, SigIR] {
    const sigIR = this.sigToIR(sig);
    const toDestructure = getInterfaceDeclToDestructure(sig);
    if (!toDestructure) {
      return [sigIR];
    }
    // Don't destructure Array
    if (
      toDestructure.asKind(SyntaxKind.InterfaceDeclaration)?.getName() ===
      "Array"
    ) {
      return [sigIR];
    }
    const sigIRDestructured = depth2CopySig(sigIR);
    sigIRDestructured.params.pop();

    // Get the resolved property types from the actual parameter type instead of the interface definition
    const decl = sig.getDeclaration() as SignaturedDeclaration;
    const lastParam = decl.getParameters().at(-1);
    const paramType = lastParam?.getType();

    const getPropType = (prop: PropertySignature): TypeIR => {
      const name = prop.getName();
      const optional = !!prop.getQuestionTokenNode();
      // Try to get resolved type from the parameter type, fallback to interface definition
      let type: TypeIR = this.typeToIR(prop.getTypeNode()!, optional);
      const propSymbol = paramType?.getProperty(name);
      if (!propSymbol) {
        return type;
      }
      const propType = propSymbol.getTypeAtLocation(lastParam!);
      // Convert the resolved type by getting a dummy type node
      // This is a bit hacky. Would be nice to create a synthetic node more
      // directly.
      const tempType = propType.getText();
      if (tempType === prop.getTypeNode()?.getText()) {
        return type;
      }

      if (propType.isTypeParameter()) {
        return { kind: "parameterReference", name: tempType };
      }
      // Create a dummy type node from the resolved type text
      const project = lastParam!.getProject();

      // Include type parameters in the temporary type definition
      const sigTypeParams = sigIR.typeParams || [];
      // Also include class-level type parameters
      const allTypeParams = [
        ...sigTypeParams,
        ...Array.from(this.classTypeParams),
      ];
      const typeParamsString =
        allTypeParams.length > 0 ? `<${allTypeParams.join(", ")}>` : "";
      const tempFile = project.createSourceFile(
        `__temp__${Math.random()}.ts`,
        `type Temp${typeParamsString} = ${tempType};`,
      );
      const typeAliasDecl = tempFile.getTypeAliases()[0];
      const dummyTypeNode = typeAliasDecl.getTypeNode()!;
      const res = this.typeToIR(dummyTypeNode, optional);
      // Don't remove the temp file, it causes crashes. TODO: Fix this?
      return res;
    };
    const kwargs: ParamIR[] = toDestructure.getProperties().map((prop) => {
      const name = prop.getName();
      const isOptional = !!prop.getQuestionTokenNode();
      const type = getPropType(prop);
      return { name, type, isOptional };
    });
    sigIRDestructured.kwparams = kwargs;
    if (toDestructure.isKind(SyntaxKind.TypeLiteral)) {
      return [sigIRDestructured];
    }
    return [sigIR, sigIRDestructured];
  }

  callableToIR(
    name: string,
    signatures: Signature[],
    isStatic?: boolean,
  ): CallableIR {
    this.pushNameContext(name);
    const sigs = signatures.flatMap((sig, idx) => {
      this.pushNameContext(`Sig${idx}`);
      const result = this.sigToIRDestructure(sig);
      this.popNameContext();
      return result;
    });
    this.popNameContext();

    const result: CallableIR = {
      kind: "callable",
      name,
      signatures: sigs,
      isStatic,
    };

    return result;
  }

  getTypeParamsFromDecl<T extends TypeParameteredNode>(decl: T): string[] {
    return getFilteredTypeParams(decl)
      .filter(
        (p) =>
          // Filter out type parameters that are already declared at class level
          !this.classTypeParams.has(p.getName()),
      )
      .map((p) => p.getName());
  }

  getTypeParamsFromDecls<T extends TypeParameteredNode>(decls: T[]): string[] {
    return decls.flatMap((decl) => this.getTypeParamsFromDecl(decl));
  }

  funcDeclsToIR(name: string, decls: FunctionDeclaration[]): CallableIR {
    const astSigs = decls.map((x) => x.getSignature());
    const origNameContext = this.nameContext;
    this.nameContext ??= [];
    const result = this.callableToIR(name, astSigs, false);
    this.nameContext = origNameContext;
    return result;
  }

  propertySignatureToIR(
    member: PropertySignature | PropertyDeclaration,
    isStatic: boolean = false,
  ): PropertyIR {
    const name = member.getName();
    const isOptional = member.hasQuestionToken();
    this.pushNameContext(name);
    const type = this.typeToIR(member.getTypeNode()!, isOptional);
    this.popNameContext();
    const isReadonly = member.isReadonly();
    return { name, type, isOptional, isStatic, isReadonly };
  }

  interfaceToIR(
    name: string,
    bases: BaseIR[],
    members: Node[],
    staticMembers: Node[],
    callSignatures: CallSignatureDeclaration[],
    typeParams: string[],
  ): InterfaceIR {
    // Set class-level type parameters before processing methods
    for (const param of typeParams) {
      this.classTypeParams.add(param);
    }
    try {
      const { methods: astMethods, properties: astProperties } =
        groupMembers(members);
      const {
        methods: staticAstMethods,
        properties: staticAstProperties,
        constructors,
      } = groupMembers(staticMembers);
      const propMap = new Map(
        astProperties.map((prop) => [prop.getName(), prop]),
      );
      for (const key of Object.keys(staticAstMethods)) {
        delete astMethods[key];
      }
      typeParams = Array.from(new Set(typeParams));
      const extraMethods: CallableIR[] = [];

      // If we extend record, add a __getattr__ impl as appropriate.
      let record1: BaseIR | undefined;
      [[record1 = undefined], bases] = split2(
        bases,
        (x) => x.name === "Record",
      );
      const record = record1 as BaseIR | undefined;
      if (record) {
        extraMethods.push({
          kind: "callable",
          name: "__getattr__",
          signatures: [
            {
              params: [
                { name: "key", type: simpleType("str"), isOptional: false },
              ],
              returns: record.typeArgs[1],
            },
          ],
        });
      }
      if ("[Symbol.iterator]" in astMethods) {
        const x = astMethods["[Symbol.iterator]"];
        delete astMethods["[Symbol.iterator]"];
        const typeNode = x[0]
          .getDeclaration()
          .getReturnTypeNode()!
          .asKindOrThrow(SyntaxKind.TypeReference);
        const returns = this.typeToIR(typeNode);
        if (returns.kind === "parameterReference") {
          throw new Error("Cannot happen!");
        }
        returns.name = "PyIterator";
        returns.typeArgs = [returns.typeArgs[0]];
        extraMethods.push({
          kind: "callable",
          name: "__iter__",
          signatures: [{ params: [], returns }],
        });
      }
      if (
        name !== "Function_iface" &&
        (propMap.get("size")?.getTypeNode()?.getText() === "number" ||
          propMap.get("length")?.getTypeNode()?.getText() === "number")
      ) {
        extraMethods.push({
          kind: "callable",
          name: "__len__",
          signatures: [{ params: [], returns: simpleType("int") }],
        });
      }
      const redirectMethod = (origName: string, newName: string): void => {
        if (!(origName in astMethods)) {
          return;
        }
        extraMethods.push(this.callableToIR(newName, astMethods[origName]));
      };
      redirectMethod("has", "__contains__");
      redirectMethod("includes", "__contains__");
      redirectMethod("get", "__getitem__");
      redirectMethod("set", "__setitem__");
      redirectMethod("delete", "__delitem__");
      if (constructors) {
        staticAstMethods["new"] = constructors.map((decl) =>
          decl.getSignature(),
        );
      }
      const irMethods = ([] as CallableIR[]).concat(
        Object.entries(astMethods).map(([name, sigs]) =>
          this.callableToIR(name, sigs, false),
        ),
        Object.entries(staticAstMethods).map(([name, sigs]) =>
          this.callableToIR(name, sigs, true),
        ),
        extraMethods,
      );
      irMethods.push({
        kind: "callable",
        name: "__call__",
        signatures: callSignatures.flatMap((sig) =>
          this.sigToIRDestructure(sig.getSignature()),
        ),
        isStatic: false,
      });
      const irProps = ([] as PropertyIR[]).concat(
        astProperties.map((prop) => this.propertySignatureToIR(prop, false)),
        staticAstProperties.map((prop) =>
          this.propertySignatureToIR(prop, true),
        ),
      );
      const props = uniqBy(irProps, ({ name }) => name);
      return {
        kind: "interface",
        methods: irMethods,
        properties: props,
        name,
        typeParams,
        bases,
      };
    } finally {
      // Restore previous class type parameters
      this.classTypeParams.clear();
    }
  }

  classToIR(classDecl: ClassDeclaration): [InterfaceIR, InterfaceIR] {
    const name = classDecl.getName() || "";
    const ifaceName = name + "_iface";

    // Extract properties and methods from class members
    const allProperties = classDecl.getProperties();
    const allMethods = classDecl.getMethods();
    const typeParams = this.getTypeParamsFromDecl(classDecl);
    const bases = this.getBasesOfDecls([classDecl]);
    const [staticMethods, methods] = split2(allMethods, (x) => x.isStatic());
    const [staticProperties, properties] = split2(allProperties, (x) =>
      x.isStatic(),
    );
    const typeArgs = typeParams.map((param) => simpleType(param));
    const concreteBases = [{ name: ifaceName, typeArgs }];
    const constructors = classDecl.getConstructors();
    this.nameContext = [name];
    const ifaceIR = this.interfaceToIR(
      ifaceName,
      bases,
      [...methods, ...properties],
      [],
      [],
      typeParams,
    );
    const concreteIR = this.interfaceToIR(
      name,
      concreteBases,
      [],
      [...constructors, ...staticMethods, ...staticProperties],
      [],
      typeParams,
    );
    this.nameContext = undefined;
    return [ifaceIR, concreteIR];
  }

  getBasesOfDecls(
    decls: (InterfaceDeclaration | ClassDeclaration)[],
  ): BaseIR[] {
    let extends_ = decls.flatMap((decl) => decl.getExtends() || []);
    extends_ = uniqBy(extends_, (base) => base.getText());
    return extends_.flatMap((extend): BaseIR | [] => {
      let ident = extend.getExpression();
      if (!Node.isIdentifier(ident)) {
        return [];
      }
      let name = extend.getExpression().getText();
      const astArgs = getExpressionTypeArgs(ident, extend);
      const typeArgs = astArgs.map((node) => this.typeToIR(node, false));
      // Record is a special case. We don't want to addNeededInterface it, we'll
      // convert it to a __getattr__ impl.
      if (name === "Record") {
        return { name, typeArgs };
      }
      name += "_iface";
      this.addNeededInterface(ident as Identifier);
      return { name, typeArgs };
    });
  }

  membersDeclarationToIR(
    name: string,
    type: { getMembers: TypeLiteralNode["getMembers"] },
    bases: BaseIR[] = [],
    typeParams: string[],
  ): InterfaceIR {
    const [prototypes, staticMembers] = split(
      type.getMembers(),
      (m): m is PropertySignature =>
        m.isKind(SyntaxKind.PropertySignature) && m.getName() === "prototype",
    );
    let members: TypeElementTypes[] = [];

    // Collect type parameters from interfaces referenced in prototypes
    let inheritedTypeParams: string[] = [];
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
      const name = ident.getText() + "_iface";
      const typeArgs = getInterfaceTypeArgs(ident);
      this.addNeededInterface(ident);
      bases.push({ name, typeArgs });

      // Extract type parameters from the referenced interface
      const interfaceDefs = ident
        .getDefinitionNodes()
        .filter(Node.isInterfaceDeclaration);
      for (const interfaceDef of interfaceDefs) {
        const interfaceTypeParams = getFilteredTypeParams(interfaceDef).map(
          (p) => p.getName(),
        );
        inheritedTypeParams.push(...interfaceTypeParams);
      }
    }

    // Use inherited type parameters if none were explicitly provided
    if (typeParams.length === 0) {
      typeParams = [...new Set(inheritedTypeParams)];
    }
    return this.interfaceToIR(
      name,
      bases,
      members,
      staticMembers,
      [],
      typeParams,
    );
  }

  typeNodeToDeclaration(name: string, typeNode: TypeNode): DeclarationIR {
    const type = this.typeToIR(typeNode, false);
    return declarationIR(name, type);
  }

  varDeclToIR(varDecl: VariableDeclaration): TopLevelIR | undefined {
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
        return this.membersDeclarationToIR(name, typeNode, [], []);
      } catch (e) {
        console.warn(varDecl.getText());
        console.warn(getNodeLocation(varDecl));
        throw e;
      }
    }
    if (Node.isTypeReference(typeNode)) {
      // This also could be a constructor like `declare X: XConstructor` where
      // XConstructor has a prototype and 'new' signatures. Or not...
      return this.varDeclOfReferenceTypeToIR(name, typeNode);
    }
    return this.typeNodeToDeclaration(name, typeNode);
  }

  varDeclOfReferenceTypeToIR(
    name: string,
    typeNode: TypeReferenceNode,
  ): TopLevelIR | undefined {
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
      this.typeNodeToDeclaration(name, typeNode);
    }
    if (classified.kind === "varDecl" || classified.kind === "interfaces") {
      const { ifaces } = classified;
      this.setIfaceTypeConstraints(ifaces);
      const typeParams = this.getTypeParamsFromDecls(ifaces);
      const result = this.membersDeclarationToIR(
        name,
        {
          getMembers: () => ifaces.flatMap((iface) => iface.getMembers()),
        },
        [],
        typeParams,
      );

      this.ifaceTypeParamConstraints.clear();

      return result;
    }
    if (classified.kind === "class") {
      return declarationIR(name, simpleType(classified.decl.getName()!));
    }
    if (classified.kind === "typeAlias") {
      return this.typeNodeToDeclaration(name, classified.decl.getTypeNode()!);
    }
    assertUnreachable(classified);
  }

  identToIRIfNeeded(ident: Identifier): TopLevelIR | undefined {
    const name = ident.getText();
    if (this.convertedSet.has(name)) {
      return undefined;
    }
    this.convertedSet.add(name);
    return this.identToIR(ident);
  }

  identToIR(ident: Identifier): TopLevelIR | undefined {
    const name = ident.getText();
    if (Node.isQualifiedName(ident)) {
      throw new Error("Qualified name!");
    }
    const classified = classifyIdentifier(ident);
    switch (classified.kind) {
      case "interfaces":
        const ifaces = classified.ifaces;
        const baseNames = this.getBasesOfDecls(ifaces);
        const typeParams = ifaces
          .flatMap((i) => i.getTypeParameters())
          .map((p) => p.getName());
        return this.interfaceToIR(
          name,
          baseNames,
          ifaces.flatMap((def) => def.getMembers()),
          [],
          ifaces.flatMap((def) => def.getCallSignatures()),
          typeParams,
        );
      case "class":
        throw new Error("Unhandled");
      case "typeAlias":
        this.nameContext = [name];
        const type = this.typeToIR(classified.decl.getTypeNode()!);
        const aliasTypeParams = classified.decl
          .getTypeParameters()
          .map((p) => p.getName());
        this.nameContext = undefined;
        return { kind: "typeAlias", name, type, typeParams: aliasTypeParams };
      case "varDecl":
        console.warn("Skipping varDecl", ident.getText());
    }
    return undefined;
  }
}

export type TopLevels = {
  callables: CallableIR[];
  decls: DeclarationIR[];
  ifaces: InterfaceIR[];
  typeAliases: TypeAliasIR[];
};

export type ConversionResult = {
  topLevels: TopLevels;
};

export function convertFiles(files: SourceFile[]): ConversionResult {
  const varDecls = files.flatMap((file) => file.getVariableDeclarations());
  const funcDecls = files.flatMap((file) => file.getFunctions());
  const classDecls = files.flatMap((file) => file.getClasses());
  return convertDecls(varDecls, funcDecls, classDecls);
}

export function convertDecls(
  varDecls: VariableDeclaration[],
  funcDecls: FunctionDeclaration[],
  classDecls: ClassDeclaration[],
): ConversionResult {
  const converter = new Converter();
  const topLevels: TopLevels = {
    callables: [],
    decls: [],
    ifaces: [],
    typeAliases: [],
  };

  function pushTopLevel(tl: TopLevelIR | undefined): void {
    if (!tl) {
      return;
    }
    switch (tl.kind) {
      case "callable":
        topLevels.callables.push(tl);
        break;
      case "declaration":
        topLevels.decls.push(tl);
        break;
      case "interface":
        topLevels.ifaces.push(tl);
        break;
      case "typeAlias":
        topLevels.typeAliases.push(tl);
        break;
    }
  }

  for (const varDecl of varDecls) {
    const name = sanitizeReservedWords(varDecl.getName());
    if (converter.convertedSet.has(name)) {
      continue;
    }
    converter.convertedSet.add(name);
    pushTopLevel(converter.varDeclToIR(varDecl));
  }
  const funcDeclsByName = groupBy(funcDecls, (decl) => decl.getName()!);
  for (const [name, decls] of Object.entries(funcDeclsByName)) {
    pushTopLevel(converter.funcDeclsToIR(name, decls));
  }
  for (const classDecl of classDecls) {
    const name = sanitizeReservedWords(classDecl.getName() || "");
    if (converter.convertedSet.has(name + "_iface")) {
      continue;
    }
    converter.convertedSet.add(name + "_iface");
    converter.convertedSet.add(name);
    converter.classToIR(classDecl).forEach(pushTopLevel);
  }
  let next: Needed | undefined;
  while ((next = popElt(converter.neededSet))) {
    if (next.type === "ident") {
      pushTopLevel(converter.identToIRIfNeeded(next.ident));
      continue;
    }
    if (next.type === "interface") {
      const ident = next.ident;
      const name = ident.getText() + "_iface";
      if (converter.convertedSet.has(name)) {
        continue;
      }
      converter.convertedSet.add(name);

      const defs = ident
        .getDefinitionNodes()
        .filter(Node.isInterfaceDeclaration);
      if (defs.length) {
        // Set type parameter constraints for interface type parameters
        converter.setIfaceTypeConstraints(defs);
        const baseNames = converter
          .getBasesOfDecls(defs)
          .filter((base) => base.name !== name);
        const typeParams = converter.getTypeParamsFromDecls(defs);
        const res = converter.interfaceToIR(
          name,
          baseNames,
          defs.flatMap((def) => def.getMembers()),
          [],
          defs.flatMap((def) => def.getCallSignatures()),
          typeParams,
        );
        pushTopLevel(res);

        // Clear interface type parameter constraints after processing interface
        converter.ifaceTypeParamConstraints.clear();
        continue;
      }
      // console.warn(ident.getDefinitionNodes().map(n => n.getText()).join("\n\n"))
      console.warn("No interface declaration for " + name);
    }
  }
  for (const tl of converter.extraTopLevels) {
    pushTopLevel(tl);
  }
  return { topLevels };
}

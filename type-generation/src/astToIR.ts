import {
  CallSignatureDeclaration,
  ClassDeclaration,
  EntityName,
  FunctionDeclaration,
  Identifier,
  InterfaceDeclaration,
  IntersectionTypeNode,
  LiteralTypeNode,
  ModuleDeclaration,
  Node,
  ParameterDeclaration,
  PropertyDeclaration,
  PropertySignature,
  Signature,
  SignaturedDeclaration,
  SourceFile,
  SyntaxKind,
  ts,
  TypeElementMemberedNode,
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
import { BUILTIN_NAMES, TYPE_TEXT_MAP } from "./adjustments.js";
import { groupBy, popElt, split, split2 } from "./groupBy.js";
import {
  assertUnreachable,
  classifyIdentifier,
  deduplicateBy,
  getExpressionTypeArgs,
  getNodeLocation,
  groupMembers,
  isValidPythonIdentifier,
} from "./astUtils.js";
import { sanitizeReservedWords, uniqBy } from "./irToString.js";
import { Needed } from "./types.js";
import {
  TypeIR,
  simpleType,
  SigIR,
  ANY_IR,
  CallableIR,
  DeclarationIR,
  InterfaceIR,
  OtherTypeIR,
  ParamIR,
  ParameterReferenceTypeIR,
  PropertyIR,
  ReferenceTypeIR,
  TopLevelIR,
  TypeAliasIR,
  TypeOperatorTypeIR,
  arrayType,
  declarationIR,
  parameterReferenceType,
  parenType,
  referenceType,
  tupleType,
  unionType,
  visitType,
  replaceType,
  IRVisitor,
  visitTopLevel,
  typeParam,
  TypeParamIR,
  spreadType,
} from "./ir.js";
import { logger } from "./logger.js";

export function literalType(text: string): TypeIR {
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

export function literalTypeToIR(typeNode: LiteralTypeNode): TypeIR {
  return literalType(typeNode.getText());
}

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

/**
 * Helper for sigToIRDestructure
 *
 * If the parameter of sig is an Interface, from Python we allow the interface
 * entries to be passed as keyword arguments. Return the InterfaceDeclaration if
 * the last parameter is an interface, else return undefined.
 */
function getInterfaceDeclToDestructure(
  sig: Signature,
): [InterfaceDeclaration | TypeLiteralNode, TypeNode[]] | undefined {
  const decl = sig.getDeclaration() as SignaturedDeclaration;
  const typeNode = decl.getParameters().at(-1)?.getTypeNode();
  if (!typeNode) {
    return undefined;
  }

  // Handle inline object types (TypeLiteral)
  const typeLiteral = typeNode.asKind(SyntaxKind.TypeLiteral);
  if (typeLiteral) {
    return [typeLiteral, []];
  }
  // If it's a type parameter, don't try to destructure.
  if (typeNode.getType().isTypeParameter()) {
    return undefined;
  }
  // Handle named interface types (TypeReference)
  const refNode = typeNode.asKind(SyntaxKind.TypeReference);
  if (!refNode) {
    return undefined;
  }
  const ident = refNode.getTypeName().asKind(SyntaxKind.Identifier);
  if (!ident) {
    return undefined;
  }
  const classified = classifyIdentifier(ident);
  if (classified.kind !== "interfaces") {
    return undefined;
  }
  const iface = classified.ifaces[0];
  // Ad hoc special case: the Iterable interface is definitely not a collection
  // of options we want to destructure. Maybe we can eventually figure out a
  // better rule to exclude these?
  const name = iface.getName();
  if (name === "Iterable" || name === "AsyncIterable") {
    return undefined;
  }
  return [iface, refNode.getTypeArguments()];
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
  ).map(parameterReferenceType);
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
    nodes: (TypeElementMemberedNode & Node)[],
    modifiers: Modifier,
  ): ReferenceTypeIR {
    const res = nodes.filter(
      (x): x is TypeElementMemberedNode & TypeParameteredNode & Node =>
        Node.isTypeParametered(x),
    );
    const typeParams = this.converter.getTypeParamsFromDecls(res);

    let name = this.nameContext.join("__");
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
    const result = this.converter.interfaceToIR(
      name,
      [],
      members,
      [],
      [],
      typeParams,
    );
    if (partial) {
      for (const prop of result.properties) {
        prop.isOptional = true;
      }
    }
    this.converter.addExtraTopLevel(result);
    return referenceType(name);
  }

  classifiedTypeToIr(
    typeRoot: SyntheticTypeRoot,
    modifiersArg?: Modifier,
  ): TypeIR {
    let modifiers = modifiersArg ?? { partial: false };
    switch (typeRoot.kind) {
      case "intersection": {
        const node = typeRoot.node;
        for (const ty of node.getTypeNodes()) {
          if (ty.getType().isTypeParameter()) {
            // Can't intersect with a type parameter...
            return ANY_IR;
          }
          if (
            Node.isTypeReference(ty) &&
            Node.isQualifiedName(ty.getTypeName())
          ) {
            // We don't handle QualifiedNames yet
            return ANY_IR;
          }
        }
        const types = node
          .getTypeNodes()
          .map((ty, idx) => {
            this.nameContext.push(`Intersection${idx}`);
            // In TS it's just fine to use the LHS of a type alias in an
            // intersection. However, in Python we can't inherit from a type
            // alias. So if the reference target is a type alias, we have to
            // unwind it until we hit something we can inherit.
            while (Node.isTypeReference(ty)) {
              const classified = classifyIdentifier(
                ty.getTypeName() as Identifier,
              );
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
        const name = this.nameContext.join("__");
        this.converter.addExtraTopLevel(
          this.converter.interfaceToIR(name, types, [], [], [], []),
        );
        return referenceType(name);
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
      const classified = classifyIdentifier(n.getTypeName() as Identifier);
      if (classified.kind === "typeAlias") {
        const result = this.typeToIR(classified.decl.getTypeNode()!, modifiers);
        if (result.kind === "reference") {
          const args = n
            .getTypeArguments()
            .map((x) => this.converter.typeToIR(x));
          result.typeArgs = args;
        }
        return result;
      }
      if (classified.kind === "interfaces") {
        const { name, ifaces } = classified;
        this.nameContext.push(name);
        const result = this.doConversion(ifaces, modifiers);
        this.nameContext.pop();
        const args = n
          .getTypeArguments()
          .map((x) => this.converter.typeToIR(x));
        result.typeArgs = args;
        return result;
      }
      throw new Error(`Not handled ${classified.kind}`);
    }
    if (Node.isMappedTypeNode(n)) {
      return ANY_IR;
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
  nameToExtraTopLevel: Map<string, TopLevelIR>;
  nameContext: string[] | undefined;
  adjustedIfaces: AdjustedIfaces;

  constructor() {
    this.ifaceTypeParamConstraints = new Map();
    this.classTypeParams = new Set();
    this.neededSet = new Set();
    this.convertedSet = new Set(BUILTIN_NAMES);
    this.extraTopLevels = [];
    this.nameContext = undefined;
    this.adjustedIfaces = new Map();
    this.nameToExtraTopLevel = new Map();
  }

  addExtraTopLevel(tl: TopLevelIR) {
    addMissingTypeParametersToIface(tl, this.adjustedIfaces);
    this.nameToExtraTopLevel.set(tl.name!, tl);
    this.extraTopLevels.push(tl);
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

      return parameterReferenceType(name);
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
    return referenceType(name, typeArgs);
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
    const res = new SyntheticTypeConverter(this).classifiedTypeToIr(typeRoot);
    return res;
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
      let name = "";
      if (this.nameContext) {
        name = this.nameContext.join("__");
      }
      const iface = this.interfaceToIR(name, [], [], [], [typeNode], []);
      const ir = iface.methods[0];
      const sigs = ir.signatures;
      // Can we use an inline Callable[]? This works if it only has position
      // only parameters.
      if (
        sigs.length === 1 &&
        sigs[0].kwparams === undefined &&
        sigs[0].spreadParam === undefined
      ) {
        delete ir.name;
        delete ir.isStatic;
        return ir;
      }
      // If there's no name, we can't generate a new type.
      if (name === "") {
        logger.error(typeNode.print());
        logger.error(getNodeLocation(typeNode));
        throw new Error("Oops...");
      }
      this.addExtraTopLevel(iface);
      const ref = referenceType(name);
      return ref;
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
      this.pushNameContext("array");
      const eltType = this.typeToIR(typeNode.getElementTypeNode());
      this.popNameContext();
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

  paramToIR(
    param: ParameterDeclaration,
    idx: number,
    isLast: boolean,
  ): ParamIR | undefined {
    const optional = !!param.hasQuestionToken();
    let name = param.getName();
    let isIdentifier = isValidPythonIdentifier(name);
    const oldNameContext = this.nameContext?.slice();
    const paramType = param.getTypeNode()!;
    const destructureOnly = isLast && Node.isTypeLiteral(paramType);
    if (!destructureOnly && !isIdentifier) {
      // Replace name with args${idx}. This is an unfortunate case so we log it.
      logger.warn("Encountered argument with non identifier name");
      logger.warn(param.print());
      logger.warn(getNodeLocation(param));
      name = `args${idx}`;
      isIdentifier = true;
    }
    if (destructureOnly) {
      // If it's the last argument and the type is a type literal, we'll
      // destructure it so don't make a type.
      this.nameContext = undefined;
    } else if (isIdentifier) {
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
    return pyParam;
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
        const isLast = idx === params.length - 1;
        const spread = !!param.getDotDotDotToken();
        const pyParam = this.paramToIR(param, idx, isLast);
        if (!pyParam) {
          continue;
        }
        if (spread) {
          spreadParam = pyParam;
        } else {
          pyParams.push(pyParam);
        }
      }
      if (spreadParam) {
        const type = spreadParam.type;
        if (type.kind === "array") {
          spreadParam.type = type.type;
        } else if (type.kind === "parameterReference") {
          spreadParam.type = spreadType(type);
        } else {
          logger.warn(`expected type array for spread param, got ${type.kind}`);
          spreadParam.type = type;
        }
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
          .map((x) => parameterReferenceType(x.getName()));
        returns = referenceType(name, typeArgs);
      }

      // Extract type parameters for this specific signature
      const typeParams = this.getTypeParamsFromDecl(decl);
      const result: SigIR = { params: pyParams, spreadParam, returns };
      if (typeParams.length > 0) {
        result.typeParams = typeParams;
      }
      return result;
    } catch (e) {
      logger.warn("failed to convert", sig.getDeclaration().getText());
      logger.warn(getNodeLocation(sig.getDeclaration()));
      throw e;
    }
  }

  /**
   * If the last parameter of sig is an interface, return a pair of signatures:
   * 1. the original signature unaltered
   * 2. a signature where the entries of the last parameter are passed as key word
   *    arguments.
   */
  sigToIRDestructure(sig: Signature): [SigIR] | [SigIR, SigIR] {
    const sigIR = this.sigToIR(sig);
    const [toDestructure, typeArgs] = getInterfaceDeclToDestructure(sig) ?? [
      undefined,
      [],
    ];
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
    let typeParams: TypeParameterDeclaration[] = [];
    if (Node.isInterfaceDeclaration(toDestructure)) {
      typeParams = toDestructure.getTypeParameters();
    }
    const params = new Map(
      typeParams.map((x, idx) => [
        x.getName(),
        typeArgs[idx] ?? x.getDefault(),
      ]),
    );

    const getPropType = (prop: PropertySignature): TypeIR => {
      const optional = !!prop.getQuestionTokenNode();
      let res = this.typeToIR(prop.getTypeNode()!, optional);
      const converter = this;
      addMissingTypeArgsToType(res, this.adjustedIfaces);
      // Clone res before mutating it!
      res = structuredClone(res);
      visitType(
        {
          *visitParameterReferenceType(a) {
            const param = params.get(a.name);
            if (!param) {
              return;
            }
            replaceType(a, converter.typeToIR(param));
          },
        },
        res,
      );
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

  getTypeParamsFromDecl<T extends TypeParameteredNode>(decl: T): TypeParamIR[] {
    return getFilteredTypeParams(decl)
      .filter(
        (p) =>
          // Filter out type parameters that are already declared at class level
          !this.classTypeParams.has(p.getName()),
      )
      .map((p) => typeParam(p.getName()));
  }

  getTypeParamsFromDecls<T extends TypeParameteredNode>(
    decls: T[],
  ): TypeParamIR[] {
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

  topLevelInterfaceToIR(
    name: string,
    defs: InterfaceDeclaration[],
  ): InterfaceIR {
    // Set type parameter constraints for interface type parameters
    this.setIfaceTypeConstraints(defs);
    const baseNames = this.getBasesOfDecls(defs).filter(
      (base) => base.name !== name,
    );
    const typeParams = this.getTypeParamsFromDecls(defs);
    this.nameContext = [name];
    const res = this.interfaceToIR(
      name,
      baseNames,
      defs.flatMap((def) => def.getMembers()),
      [],
      defs.flatMap((def) => def.getCallSignatures()),
      typeParams,
    );
    this.nameContext = undefined;

    // Clear interface type parameter constraints after processing interface
    this.ifaceTypeParamConstraints.clear();
    return res;
  }

  interfaceToIR(
    name: string,
    bases: ReferenceTypeIR[],
    members: Node[],
    staticMembers: Node[],
    callSignatures: Pick<CallSignatureDeclaration, "getSignature">[],
    typeParams: TypeParamIR[],
  ): InterfaceIR {
    // Set class-level type parameters before processing methods
    for (const param of typeParams) {
      this.classTypeParams.add(param.name);
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
      typeParams = deduplicateBy(typeParams, (x) => x.name);
      const extraMethods: CallableIR[] = [];

      // If we extend record, add a __getattr__ impl as appropriate.
      let record1: ReferenceTypeIR | undefined;
      [[record1 = undefined], bases] = split2(
        bases,
        (x) => x.name === "Record",
      );
      const record = record1 as ReferenceTypeIR | undefined;
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
      if (constructors.length > 0) {
        staticAstMethods["new"] = constructors.map((decl) =>
          decl.getSignature(),
        );
      }
      const irMethods = ([] as CallableIR[]).concat(
        Object.entries(astMethods)
          .filter(([name, sigs]) => isValidPythonIdentifier(name))
          .map(([name, sigs]) => this.callableToIR(name, sigs, false)),
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
        astProperties
          .filter((x) => isValidPythonIdentifier(x.getName()))
          .map((prop) => this.propertySignatureToIR(prop, false)),
        staticAstProperties
          .filter((x) => isValidPythonIdentifier(x.getName()))
          .map((prop) => this.propertySignatureToIR(prop, true)),
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
    const typeArgs = typeParams.map((x) => parameterReferenceType(x.name));
    const concreteBases = [referenceType(ifaceName, typeArgs)];
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
    concreteIR.jsobject = true;
    this.nameContext = undefined;
    return [ifaceIR, concreteIR];
  }

  getBasesOfDecls(
    decls: (InterfaceDeclaration | ClassDeclaration)[],
  ): ReferenceTypeIR[] {
    let extends_ = decls.flatMap((decl) => decl.getExtends() || []);
    extends_ = uniqBy(extends_, (base) => base.getText());
    return extends_.flatMap((extend): ReferenceTypeIR | [] => {
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
        return referenceType(name, typeArgs);
      }
      name += "_iface";
      this.addNeededInterface(ident as Identifier);
      return referenceType(name, typeArgs);
    });
  }

  membersDeclarationToIR(
    name: string,
    type: { getMembers: TypeLiteralNode["getMembers"] },
    bases: ReferenceTypeIR[] = [],
    typeParams: TypeParamIR[],
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
      if (Node.isTypeReference(typeNode)) {
        // Handled below
      } else if (Node.isArrayTypeNode(typeNode)) {
        // One-off special case: ArrayConstructor has prototype any[]. Not sure
        // if we need to do anything here? Code gen looks alright.
        continue;
      } else {
        logger.warn(
          "Excepted prototype type to be TypeReference",
          proto.getText(),
          getNodeLocation(typeNode!),
        );
        continue;
      }
      const ident = typeNode.getTypeName() as Identifier;
      const name = ident.getText() + "_iface";
      const typeArgs = getInterfaceTypeArgs(ident);
      this.addNeededInterface(ident);
      bases.push(referenceType(name, typeArgs));

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
      typeParams = Array.from(new Set(inheritedTypeParams), (x) =>
        typeParam(x),
      );
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
        const res = this.membersDeclarationToIR(name, typeNode, [], []);
        res.jsobject = true;
        return res;
      } catch (e) {
        logger.warn(varDecl.getText());
        logger.warn(getNodeLocation(varDecl));
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
      logger.warn(ident.getText());
      return undefined;
    }

    const classified = classifyIdentifier(ident);
    if (classified.kind === "varDecl" && name !== classified.name) {
      // We have to check that name !== typeName or else we can pick up the decl
      // we're currently processing.
      return this.typeNodeToDeclaration(name, typeNode);
    }
    if (classified.kind === "varDecl" || classified.kind === "interfaces") {
      const { ifaces } = classified;
      this.setIfaceTypeConstraints(ifaces);
      const typeParams = this.getTypeParamsFromDecls(ifaces);
      this.nameContext = [name];
      const result = this.membersDeclarationToIR(
        name,
        {
          getMembers: () => ifaces.flatMap((iface) => iface.getMembers()),
        },
        [],
        typeParams,
      );
      this.nameContext = undefined;
      result.jsobject = true;

      this.ifaceTypeParamConstraints.clear();

      return result;
    }
    if (classified.kind === "class") {
      return declarationIR(name, referenceType(classified.decl.getName()!));
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
        return this.topLevelInterfaceToIR(name, classified.ifaces);
      case "class":
        throw new Error("Unhandled");
      case "typeAlias":
        this.nameContext = [name];
        const aliasTypeParams = classified.decl
          .getTypeParameters()
          .map((p) => typeParam(p.getName()));
        const typeNode = classified.decl.getTypeNode()!;
        const type = this.typeToIR(typeNode);
        this.nameContext = undefined;
        // If we just emitted a class definition with the same name, we can drop
        // the type alias.
        if (type.kind === "reference" && type.name === name) {
          const tl = this.nameToExtraTopLevel.get(name);
          if (tl?.kind === "interface") {
            tl.typeParams = aliasTypeParams;
          }
          return undefined;
        }
        return { kind: "typeAlias", name, type, typeParams: aliasTypeParams };
      case "varDecl":
        logger.warn("Skipping varDecl", ident.getText());
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
  const cf = files.flatMap((file) => file.getModule("Cloudflare") ?? []);
  return convertDecls(varDecls, funcDecls, classDecls, cf[0]);
}

export function convertDecls(
  varDecls: VariableDeclaration[],
  funcDecls: FunctionDeclaration[],
  classDecls: ClassDeclaration[],
  cf: ModuleDeclaration | undefined = undefined,
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

  // Adhoc logic to convert Cloudflare Env interface if present
  if (cf) {
    const env = cf.getInterface("Env");
    if (env) {
      pushTopLevel(converter.topLevelInterfaceToIR("Env", [env]));
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
        pushTopLevel(converter.topLevelInterfaceToIR(name, defs));
        continue;
      }
      // console.warn(ident.getDefinitionNodes().map(n => n.getText()).join("\n\n"))
      logger.warn("No interface declaration for " + name);
    }
  }
  for (const tl of converter.extraTopLevels) {
    pushTopLevel(tl);
  }
  const flattenedTopLevels: TopLevelIR[] = Object.values(topLevels).flatMap(
    (x) => x as TopLevelIR[],
  );
  for (const tl of flattenedTopLevels) {
    addMissingTypeArgsToTopLevel(tl, converter.adjustedIfaces);
  }
  return { topLevels };
}

type AdjustedIfaces = Map<string, { nparams: number; added: string[] }>;

class TypeParamContext {
  typeParams: Map<string, TypeParamIR>[];
  constructor() {
    this.typeParams = [];
  }

  push(
    tps: Iterable<TypeParamIR> | Map<string, TypeParamIR> | undefined,
  ): void {
    let res: Map<string, TypeParamIR>;
    if (tps instanceof Map) {
      res = tps;
    } else {
      res = new Map(Array.from(tps ?? [], (x) => [x.name, x]));
    }
    this.typeParams.push(res);
  }

  pop(): void {
    this.typeParams.pop();
  }

  get(x: string): TypeParamIR | undefined {
    for (const p of this.typeParams) {
      const res = p.get(x);
      if (res) {
        return res;
      }
    }
    return undefined;
  }

  has(x: string): boolean {
    for (const p of this.typeParams) {
      if (p.has(x)) {
        return true;
      }
    }
    return false;
  }
}

function addMissingTypeParametersToIface(
  topLevel: TopLevelIR,
  adjustedIfaces: AdjustedIfaces,
): void {
  const context = new TypeParamContext();
  const missingTypeParams: Set<string> = new Set();
  const visitor: IRVisitor = {
    *visitSignature(a) {
      context.push(a.typeParams);
      yield;
      context.pop();
    },
    *visitInterfaceIR(a) {
      context.push(a.typeParams);
      yield;
      context.pop();
      if (missingTypeParams.size) {
        a.typeParams.push(
          ...Array.from(missingTypeParams, (x) => typeParam(x)),
        );
        const nparams = a.typeParams.length;
        adjustedIfaces.set(a.name, {
          nparams,
          added: Array.from(missingTypeParams),
        });
        missingTypeParams.clear();
      }
    },
    *visitTypeAliasIR(a) {
      context.push(a.typeParams);
      yield;
      context.pop();
      if (missingTypeParams.size) {
        throw new Error("Not implemented");
      }
    },
    *visitParameterReferenceType({ name }) {
      if (!context.has(name)) {
        missingTypeParams.add(name);
      }
    },
  };
  visitTopLevel(visitor, topLevel);
}

function addMissingTypeArgsVisitor(adjustedIfaces: AdjustedIfaces): IRVisitor {
  const context = new TypeParamContext();
  return {
    *visitSignature(a) {
      // Sometimes the signature has the same type parameter as an ambient
      // class. Remove these duplicates.
      const s = new Map(a.typeParams?.map((x) => [x.name, x]));
      let removedTypeParam = false;
      for (const x of a.typeParams ?? []) {
        if (context.has(x.name)) {
          s.delete(x.name);
          removedTypeParam = true;
        }
      }
      if (removedTypeParam) {
        a.typeParams = Array.from(s.values());
      }
      context.push(s);
      yield;
      context.pop();
    },
    *visitInterfaceIR(a) {
      context.push(a.typeParams);
      yield;
      context.pop();
    },
    *visitTypeAliasIR(a) {
      context.push(a.typeParams);
      yield;
      context.pop();
    },
    *visitSpreadType(a) {
      if (a.type.kind === "parameterReference") {
        const name = a.type.name;
        const target = context.get(name);
        if (target) {
          target.spread = true;
        }
      }
    },
    *visitReferenceType(rt: ReferenceTypeIR & { adjusted?: boolean }) {
      const { name, typeArgs, adjusted } = rt;
      if (adjusted) {
        return;
      }
      const { added, nparams } = adjustedIfaces.get(name) ?? { nparams: 0 };
      if (!added) {
        return;
      }
      rt.adjusted = true;
      const paramsToAdd = nparams - typeArgs.length;
      for (let i = 0; i < paramsToAdd; i++) {
        typeArgs.push(parameterReferenceType(added[i]));
      }
      for (const param of added) {
        if (!context.has(param)) {
          logger.warn("Dangling type argument", name, param);
        }
      }
    },
  };
}

function fixUpSpreadParameterReferences(tl: TopLevelIR) {
  const context = new TypeParamContext();
  let inSpread = false;
  visitTopLevel(
    {
      *visitSignature(a) {
        context.push(a.typeParams);
        yield;
        context.pop();
      },
      *visitInterfaceIR(a) {
        context.push(a.typeParams);
        yield;
        context.pop();
      },
      *visitTypeAliasIR(a) {
        context.push(a.typeParams);
        yield;
        context.pop();
      },
      *visitSpreadType(a) {
        inSpread = true;
      },
      *visitParameterReferenceType(a) {
        const isInSpread = inSpread;
        inSpread = false;
        const target = context.get(a.name);
        if (!target) {
          return;
        }
        if (target.spread && !isInSpread) {
          const replacement = spreadType(structuredClone(a));
          replaceType(a, replacement);
        }
      },
    },
    tl,
  );
}

function addMissingTypeArgsToTopLevel(
  tl: TopLevelIR,
  adjustedIfaces: AdjustedIfaces,
) {
  visitTopLevel(addMissingTypeArgsVisitor(adjustedIfaces), tl);
  fixUpSpreadParameterReferences(tl);
}

function addMissingTypeArgsToType(t: TypeIR, adjustedIfaces: AdjustedIfaces) {
  visitType(addMissingTypeArgsVisitor(adjustedIfaces), t);
}

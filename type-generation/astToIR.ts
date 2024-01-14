import {
  ClassDeclaration,
  EntityName,
  FunctionDeclaration,
  Identifier,
  InterfaceDeclaration,
  IntersectionTypeNode,
  LiteralTypeNode,
  Node,
  PropertySignature,
  Signature,
  SignaturedDeclaration,
  SourceFile,
  SyntaxKind,
  TypeElementTypes,
  TypeLiteralNode,
  TypeNode,
  TypeOperatorTypeNode,
  TypeReferenceNode,
  UnionTypeNode,
  VariableDeclaration,
} from "ts-morph";
import { BUILTIN_NAMES, TYPE_TEXT_MAP } from "./adjustments";
import { groupBy, popElt, split } from "./groupBy";
import {
  assertUnreachable,
  classifyIdentifier,
  getExpressionTypeArgs,
  getNodeLocation,
  groupMembers,
} from "./astUtils";
import { sanitizeReservedWords, uniqBy } from "./render";
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
  | CallableIR;

type SimpleTypeIR = { kind: "simple"; text: string };
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
  identName: string;
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
}: SigIR): SigIR {
  params = Array.from(params);
  kwparams = kwparams && Array.from(kwparams);
  return { params, spreadParam, kwparams, returns };
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
  extraBases?: string[];
  concrete?: boolean;
};

export type BaseIR = {
  name: string;
  typeParams: TypeIR[];
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
};

export type TopLevelIR = DeclarationIR | InterfaceIR | TypeAliasIR | CallableIR;

function simpleType(text: string): SimpleTypeIR {
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
 * Helper for getting the bases in membersDeclarationToIR
 */
function getInterfaceTypeParams(ident: EntityName): TypeIR[] {
  return uniqBy(
    (ident as Identifier)
      .getDefinitionNodes()
      .filter(Node.isInterfaceDeclaration)
      .flatMap((def) => def.getTypeParameters())
      .map((param) => param.getName()),
    (param) => param,
  ).map((name) => ({ kind: "parameterReference", name }));
}

const operatorToName = {
  [SyntaxKind.ReadonlyKeyword]: "readonly",
  [SyntaxKind.UniqueKeyword]: "unique",
};

export class Converter {
  typeParams: Set<string>;
  neededSet: Set<Needed>;
  convertedSet: Set<string>;

  constructor() {
    this.typeParams = new Set();
    this.neededSet = new Set();
    this.convertedSet = new Set(BUILTIN_NAMES);
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

  typeOperatorToIR(typeNode: TypeOperatorTypeNode): TypeOperatorTypeIR {
    const operator = typeNode.getOperator();
    const operatorName = operatorToName[operator];
    if (!operatorName) {
      throw new Error("Unknown type operator " + operator);
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
    const types = rest.map((ty) => this.typeToIR(ty, false));
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

  intersectionToIR(typeNode: IntersectionTypeNode) {
    const filteredTypes = typeNode
      .getTypeNodes()
      .filter(
        (type) =>
          !(
            Node.isThisTypeNode(type) || type.getText().startsWith("ThisType<")
          ),
      );
    if (filteredTypes.length === 1) {
      return this.typeToIR(filteredTypes[0]);
    }
    const typeString = typeNode.getType().getText();
    if (typeString === "Window & typeof globalThis") {
      return ANY_IR;
    }
    if (typeString === "ArrayBufferLike & { BYTES_PER_ELEMENT?: never; }") {
      return simpleType("ArrayBuffer");
    }
    const location = getNodeLocation(typeNode);
    console.warn("No conversion for intersection at " + location);
    return ANY_IR;
  }

  typeReferenceToIR(typeNode: TypeReferenceNode): TypeIR {
    const ident = typeNode.getTypeName();
    if (typeNode.getType().isTypeParameter()) {
      const name = ident.getText();
      this.typeParams.add(name);
      return { kind: "parameterReference", name };
    }
    const typeArgs = getExpressionTypeArgs(ident, typeNode).map((ty) =>
      this.typeToIR(ty),
    );
    if (Node.isQualifiedName(ident)) {
      return ANY_IR;
    }
    const { name, kind } = classifyIdentifier(ident);
    if (!this.convertedSet.has(name)) {
      if (kind === "interfaces") {
        this.addNeededInterface(ident);
      } else {
        this.addNeededIdentifier(ident);
      }
    }
    return { kind: "reference", identName: name, typeArgs };
  }

  otherTypeToIR(node: Node): OtherTypeIR {
    const nodeKind = node.getKindName();
    const location = getNodeLocation(node);
    return { kind: "other", nodeKind, location };
  }

  typeToIR(
    typeNode: TypeReferenceNode,
    isOptional?: boolean,
  ): ReferenceTypeIR | ParameterReferenceTypeIR;
  typeToIR(typeNode: TypeNode, isOptional?: boolean): TypeIR;
  typeToIR(typeNode: TypeNode, isOptional: boolean = false): TypeIR {
    const typeText = typeNode.getText();
    if (typeText === "number") {
      return unionType([simpleType("int"), simpleType("float")]);
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
      return typeLiteralToIR(typeNode);
    }
    if (Node.isIntersectionTypeNode(typeNode)) {
      return this.intersectionToIR(typeNode);
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
      const elts = typeNode.getElements().map((elt) => this.typeToIR(elt));
      return tupleType(elts);
    }
    if (Node.isTypePredicate(typeNode)) {
      return simpleType("bool");
    }
    const signatures = typeNode.getType().getCallSignatures();
    if (signatures.length > 0) {
      throw new Error("oops");
    }
    return this.otherTypeToIR(typeNode);
  }

  sigToIR(sig: Signature): SigIR {
    const decl = sig.getDeclaration() as SignaturedDeclaration;
    try {
      const pyParams: ParamIR[] = [];
      let spreadParam: ParamIR;
      for (const param of decl.getParameters()) {
        const spread = !!param.getDotDotDotToken();
        const optional = !!param.hasQuestionToken();
        const type = this.typeToIR(param.getTypeNode()!, optional);
        const pyParam: ParamIR = { name: param.getName(), type, optional };
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
      const retNode = decl.getReturnTypeNode()!;
      const returns = this.typeToIR(retNode);
      return { params: pyParams, spreadParam, returns };
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
    const sigIRDestructured = depth2CopySig(sigIR);
    sigIRDestructured.params.pop();
    const kwargs: ParamIR[] = [];
    for (const prop of toDestructure.getProperties()) {
      const name = prop.getName();
      const optional = !!prop.getQuestionTokenNode();
      const type = this.typeToIR(prop.getTypeNode()!, optional);
      kwargs.push({ name, type, optional });
    }
    sigIRDestructured.kwparams = kwargs;
    return [sigIR, sigIRDestructured];
  }

  callableToIR(
    name: string,
    signatures: Signature[],
    isStatic?: boolean,
  ): CallableIR {
    const sigs = signatures.flatMap((sig) => this.sigToIRDestructure(sig));
    return { kind: "callable", name, signatures: sigs, isStatic };
  }

  funcDeclsToIR(name: string, decls: FunctionDeclaration[]): CallableIR {
    const astSigs = decls.map((x) => x.getSignature());
    return this.callableToIR(name, astSigs, false);
  }

  propertySignatureToIR(
    member: PropertySignature,
    isStatic: boolean = false,
  ): PropertyIR {
    const name = member.getName();
    const isOptional = member.hasQuestionToken();
    const type = this.typeToIR(member.getTypeNode()!, isOptional);
    const isReadonly = member.isReadonly();
    return { name, type, isOptional, isStatic, isReadonly };
  }

  interfaceToIR(
    name: string,
    bases: BaseIR[],
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
    const propMap = new Map(
      astProperties.map((prop) => [prop.getName(), prop]),
    );
    for (const key of Object.keys(staticAstMethods)) {
      delete astMethods[key];
    }
    typeParams = Array.from(new Set(typeParams));
    const extraMethods: CallableIR[] = [];
    if ("[Symbol.iterator]" in astMethods) {
      const x = astMethods["[Symbol.iterator]"];
      delete astMethods["[Symbol.iterator]"];
      const typeNode = x[0]
        .getDeclaration()
        .getReturnTypeNode()
        .asKindOrThrow(SyntaxKind.TypeReference);
      if (
        !["IterableIterator", "Iterator"].includes(
          typeNode.getTypeName().getText(),
        )
      ) {
        console.log(typeNode.getText());
        console.log(getNodeLocation(typeNode));
        throw new Error("Surprise!");
      }
      const returns = this.typeToIR(typeNode);
      if (returns.kind === "parameterReference") {
        throw new Error("Cannot happen!");
      }
      returns.identName = "PyIterator";
      returns.typeArgs = [returns.typeArgs[0]];
      extraMethods.push({
        kind: "callable",
        name: "__iter__",
        signatures: [{ params: [], returns }],
      });
    }
    if (
      name !== "Function_iface" &&
      (propMap.get("size")?.getTypeNode().getText() === "number" ||
        propMap.get("length")?.getTypeNode().getText() === "number")
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
      staticAstMethods["new"] = constructors.map((decl) => decl.getSignature());
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
    const irProps = ([] as PropertyIR[]).concat(
      astProperties.map((prop) => this.propertySignatureToIR(prop, false)),
      staticAstProperties.map((prop) => this.propertySignatureToIR(prop, true)),
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
  }

  declsToBases(decls: (InterfaceDeclaration | ClassDeclaration)[]): BaseIR[] {
    let extends_ = decls.flatMap((decl) => decl.getExtends() || []);
    extends_ = uniqBy(extends_, (base) => base.getText());
    return extends_.flatMap((extend): BaseIR | [] => {
      let ident = extend.getExpression();
      if (!Node.isIdentifier(ident)) {
        return [];
      }
      let name = extend.getExpression().getText();
      const astParams = getExpressionTypeArgs(ident, extend);
      // Unfortunately typescript doesn't expose getVariances on the type
      // checker, so we probably can't figure out what to put here.
      const irParams = astParams.map((node) => this.typeToIR(node, false));
      name += "_iface";
      this.addNeededInterface(ident as Identifier);
      return { name, typeParams: irParams };
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
      const typeParams = getInterfaceTypeParams(ident);
      this.addNeededInterface(ident);
      bases.push({ name, typeParams });
    }
    return this.interfaceToIR(name, bases, members, staticMembers, typeParams);
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
      const typeParams = ifaces
        .flatMap((i) => i.getTypeParameters())
        .map((p) => p.getName());
      return this.membersDeclarationToIR(
        name,
        {
          getMembers: () => ifaces.flatMap((iface) => iface.getMembers()),
        },
        [],
        typeParams,
      );
    }
    if (classified.kind === "class") {
      return declarationIR(name, simpleType(classified.decl.getName()));
    }
    if (classified.kind === "typeAlias") {
      return this.typeNodeToDeclaration(name, classified.decl.getTypeNode());
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
        const baseNames = this.declsToBases(ifaces);
        const typeParams = ifaces
          .flatMap((i) => i.getTypeParameters())
          .map((p) => p.getName());
        return this.interfaceToIR(
          name,
          baseNames,
          ifaces.flatMap((def) => def.getMembers()),
          [],
          typeParams,
        );
      case "class":
        throw new Error("Unhandled");
      case "typeAlias":
        const type = this.typeToIR(classified.decl.getTypeNode()!);
        return { kind: "typeAlias", name, type };
      case "varDecl":
        console.warn("Skipping varDecl", ident.getText());
    }
    return undefined;
  }
}

export type ConversionResult = {topLevels: TopLevelIR[], typeParams: Set<string> };

export function convertFiles(files: SourceFile[]): ConversionResult {
  const varDecls = files.flatMap((file) => file.getVariableDeclarations());
  const funcDecls = files.flatMap((file) => file.getFunctions());
  return convertDecls(varDecls, funcDecls);
}

export function convertDecls(varDecls: VariableDeclaration[], funcDecls: FunctionDeclaration[]): ConversionResult {
  const converter = new Converter();
  const topLevels: TopLevelIR[] = [];
  for (const varDecl of varDecls) {
    const name = sanitizeReservedWords(varDecl.getName());
    if (converter.convertedSet.has(name)) {
      continue;
    }
    converter.convertedSet.add(name);
    const result = converter.varDeclToIR(varDecl);
    if (result) {
      topLevels.push(result);
    }
  }
  const funcDeclsByName = groupBy(funcDecls, (decl) => decl.getName());
  for (const [name, decls] of Object.entries(funcDeclsByName)) {
    topLevels.push(converter.funcDeclsToIR(name, decls));
  }
  let next: Needed | undefined;
  while ((next = popElt(converter.neededSet))) {
    if (next.type === "ident") {
      let res = converter.identToIRIfNeeded(next.ident);
      if (res) {
        topLevels.push(res);
      }
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
        const baseNames = converter.declsToBases(defs).filter(
          (base) => base.name !== name,
        );
        const typeParams = defs
          .flatMap((i) => i.getTypeParameters())
          .map((p) => p.getName());
        const res = converter.interfaceToIR(
          name,
          baseNames,
          defs.flatMap((def) => def.getMembers()),
          [],
          typeParams,
        );
        topLevels.push(res);
        continue;
      }
      // console.warn(ident.getDefinitionNodes().map(n => n.getText()).join("\n\n"))
      console.warn("No interface declaration for " + name);
    }
  }
  const typeParams = converter.typeParams;
  return {topLevels, typeParams};
}

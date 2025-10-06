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

export type SimpleTypeIR = { kind: "simple"; text: string };
export type NumberTypeIR = { kind: "number" };
export type UnionTypeIR = { kind: "union"; types: TypeIR[] };
export type TupleTypeIR = { kind: "tuple"; types: TypeIR[] };
export type ArrayTypeIR = { kind: "array"; type: TypeIR };
export type ParenTypeIR = { kind: "paren"; type: TypeIR };
export type TypeOperatorTypeIR = {
  kind: "operator";
  operatorName: string;
  type: TypeIR;
};
export type OtherTypeIR = { kind: "other"; nodeKind: string; location: string };

export type ParameterReferenceTypeIR = {
  kind: "parameterReference";
  name: string;
};
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
  jsobject?: boolean;
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

export function unionType(types: TypeIR[]): UnionTypeIR {
  return { kind: "union", types };
}

export function tupleType(types: TypeIR[]): TupleTypeIR {
  return { kind: "tuple", types };
}

export function arrayType(type: TypeIR): ArrayTypeIR {
  return { kind: "array", type };
}

export function parenType(type: TypeIR): ParenTypeIR {
  return { kind: "paren", type };
}

export function declarationIR(name: string, type: TypeIR): DeclarationIR {
  return { kind: "declaration", name, type };
}

export function referenceType(
  name: string,
  typeArgs: TypeIR[] = [],
): ReferenceTypeIR {
  return { kind: "reference", name, typeArgs };
}

export function parameterReferenceType(name: string): ParameterReferenceTypeIR {
  return { kind: "parameterReference", name };
}

export const ANY_IR = simpleType("Any");

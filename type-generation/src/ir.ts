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
  | NumberTypeIR
  | SpreadTypeIR;

export type SimpleTypeIR = { kind: "simple"; text: string };
export type NumberTypeIR = { kind: "number" };
export type UnionTypeIR = { kind: "union"; types: TypeIR[] };
export type TupleTypeIR = { kind: "tuple"; types: TypeIR[] };
export type ArrayTypeIR = { kind: "array"; type: TypeIR };
export type ParenTypeIR = { kind: "paren"; type: TypeIR };
export type SpreadTypeIR = { kind: "spread"; type: TypeIR };
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
export type TypeParamIR = {
  name: string;
  spread?: boolean;
};
export type SigIR = {
  params: ParamIR[];
  spreadParam?: ParamIR;
  kwparams?: ParamIR[];
  returns: TypeIR;
  typeParams?: TypeParamIR[];
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
  typeParams: TypeParamIR[];
  bases: ReferenceTypeIR[];
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

export type DeclarationIR = {
  kind: "declaration";
  name: string;
  type: TypeIR;
};

export type TypeAliasIR = {
  kind: "typeAlias";
  name: string;
  type: TypeIR;
  typeParams: TypeParamIR[];
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

export function typeParam(name: string, isSpread?: boolean) {
  return { name, isSpread };
}

export function spreadType(type: TypeIR): SpreadTypeIR {
  return { kind: "spread", type };
}

export const ANY_IR = simpleType("Any");

export function replaceIR<T>(a: T, b: T): void {
  const c = a as any;
  for (const k in c) {
    delete c[k];
  }
  Object.assign(c, b);
}

export const replaceType = replaceIR<TypeIR>;

export type IRVisitor = {
  [Property in TypeIR["kind"] as `visit${Capitalize<Property>}Type`]?: (
    a: TypeIR & { kind: Property },
  ) => Generator<void>;
} & {
  [Property in TopLevelIR["kind"] as `visit${Capitalize<Property>}IR`]?: (
    a: TopLevelIR & { kind: Property },
  ) => Generator<void>;
} & {
  visitSignature?: (a: SigIR) => Generator<void>;
  visitParam?: (a: ParamIR) => Generator<void>;
  visitProperty?: (a: PropertyIR) => Generator<void>;
};

function enter<T>(it: Generator<void> | undefined, cb: () => T): T {
  if (!it) {
    return cb();
  }
  it?.next();
  let result;
  try {
    result = cb();
  } catch (e) {
    it.throw(e);
  }
  it?.next();
  return result!;
}

function visitParam(v: IRVisitor, a: ParamIR) {
  enter(v.visitParam?.(a), () => {
    visitType(v, a.type);
  });
}

function visitSignature(v: IRVisitor, a: SigIR) {
  enter(v.visitSignature?.(a), () => {
    for (const param of a.params) {
      visitParam(v, param);
    }
    if (a.spreadParam) {
      visitParam(v, a.spreadParam);
    }
    for (const param of a.kwparams ?? []) {
      visitParam(v, param);
    }
    visitType(v, a.returns);
  });
}

function visitProperty(v: IRVisitor, a: PropertyIR) {
  enter(v.visitProperty?.(a), () => visitType(v, a.type));
}

export function visitType(v: IRVisitor, a: TypeIR) {
  switch (a.kind) {
    case "array":
      enter(v.visitArrayType?.(a), () => visitType(v, a.type));
      return;
    case "callable":
      enter(v.visitCallableType?.(a), () => {
        for (const sig of a.signatures) {
          visitSignature(v, sig);
        }
      });
      return;
    case "number": {
      enter(v.visitNumberType?.(a), () => {});
      return;
    }
    case "operator":
      enter(v.visitOperatorType?.(a), () => visitType(v, a.type));
      return;
    case "other":
      enter(v.visitOtherType?.(a), () => {});
      return;
    case "parameterReference":
      enter(v.visitParameterReferenceType?.(a), () => {});
      return;
    case "paren":
      enter(v.visitParenType?.(a), () => visitType(v, a.type));
      return;
    case "reference":
      enter(v.visitReferenceType?.(a), () => {
        for (const ty of a.typeArgs) {
          visitType(v, ty);
        }
      });
      return;
    case "simple":
      enter(v.visitSimpleType?.(a), () => {});
      return;
    case "tuple":
      enter(v.visitTupleType?.(a), () => {
        for (const ty of a.types) {
          visitType(v, ty);
        }
      });
      return;
    case "union":
      enter(v.visitUnionType?.(a), () => {
        for (const ty of a.types) {
          visitType(v, ty);
        }
      });
      return;
    case "spread":
      enter(v.visitSpreadType?.(a), () => {
        visitType(v, a.type);
      });
      return;
  }
}

export function visitTopLevel(v: IRVisitor, a: TopLevelIR): void {
  switch (a.kind) {
    case "callable":
      enter(v.visitCallableIR?.(a), () => {
        for (const sig of a.signatures) {
          visitSignature(v, sig);
        }
      });
      return;
    case "declaration":
      enter(v.visitDeclarationIR?.(a), () => {
        visitType(v, a.type);
      });
      return;
    case "interface":
      enter(v.visitInterfaceIR?.(a), () => {
        for (const base of a.bases) {
          visitType(v, base);
        }
        for (const meth of a.methods) {
          visitTopLevel(v, meth);
        }
        for (const prop of a.properties) {
          visitProperty(v, prop);
        }
      });
      return;
    case "typeAlias":
      enter(v.visitTypeAliasIR?.(a), () => visitType(v, a.type));
      return;
  }
}

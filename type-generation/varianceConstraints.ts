import {
  InterfaceIR,
  ReferenceTypeIR,
  ArrayTypeIR,
  TypeOperatorTypeIR,
  ParameterReferenceTypeIR,
  SigIR,
  PropertyIR,
  BaseIR,
} from "./astToIR.ts";
import { IRVisitorBase, IRVisitorIface } from "./irVisitor.ts";
import { Variance } from "./types.ts";

type VarianceState = {
  state: Variance;
  readonly variables?: Map<string, 0 | 1>;
};

const Unit: VarianceState = { state: 1, variables: new Map() };
enum IntersectionVariance {
  invar = 0,
  covar = 1,
  contra = 2,
  bivar = 3,
}

function circVarianceToIntersectVariance(
  state: Variance,
): IntersectionVariance {
  if (isNaN(state)) {
    return 3;
  }
  const x = state & 3;
  return x & ~(x >> 1);
}

function intersectVarianceToCircVariance(v: IntersectionVariance): Variance {
  return [Variance.invar, Variance.covar, Variance.contra, Variance.bivar][v];
}

function xor(a: 0 | 1, b: 0 | 1): 0 | 1 {
  return (a ^ b) as 0 | 1;
}

function applyInvariantToState({
  state,
  variables,
}: VarianceState): VarianceState {
  state *= 0;
  return { state, variables };
}

function reverseVarianceState({
  state,
  variables,
}: VarianceState): VarianceState {
  state = -state;
  return { state, variables };
}

function applyVariableToState(
  { state, variables: origVariables }: VarianceState,
  varName,
): VarianceState {
  const variables = new Map(origVariables);
  variables.set(varName, xor(variables.get(varName), 1));
  return { state, variables };
}

type VarianceConstraint = {
  state: IntersectionVariance;
  variables: VarianceState[];
};

class VarianceVisitor extends IRVisitorBase implements IRVisitorIface {
  state: VarianceState;
  readonlyArray: boolean;
  readonly ifaceName: string;
  readonly constraints: VarianceConstraint[];
  readonly typeParams: readonly string[];

  constructor(t: InterfaceIR) {
    super();
    this.state = Unit;
    this.readonlyArray = false;
    this.ifaceName = t.name;
    this.constraints = Array.from({ length: t.typeParams.length }, () => ({
      state: 3,
      variables: [],
    }));
    this.typeParams = t.typeParams.map(({ name }) => name);
  }

  visitBase(t: BaseIR): boolean {
    return this.visitReferenceType(t);
  }

  visitProperty(t: PropertyIR) {
    // If a type parameter appears directly as a parameter, and it's not
    // readonly, mark it as invariant.
    if (t.isReadonly) {
      return false;
    }
    const origState = this.state;
    this.state = applyInvariantToState(origState);
    this.traverseType(t.type);
    this.state = origState;
    return true;
  }

  visitArrayType(t: ArrayTypeIR): boolean {
    const origState = this.state;
    if (!this.readonlyArray) {
      this.state = applyInvariantToState(origState);
    }
    this.readonlyArray = false;
    this.traverseType(t.type);
    this.state = origState;
    return true;
  }

  visitOperatorType(t: TypeOperatorTypeIR): boolean {
    if (t.type.kind === "array" && t.operatorName === "readonly") {
      this.readonlyArray = true;
    }
    return false;
  }

  visitParameterReferenceType(t: ParameterReferenceTypeIR): boolean {
    if (t.type === "function") {
      return;
    }
    // A class type parameter. Record it.
    const curConstraints = this.constraints[t.idx];
    if (curConstraints.state === 0) {
      // already invariant, skip
      return;
    }
    if (this.state.variables.size === 0) {
      curConstraints.state &= circVarianceToIntersectVariance(this.state.state);
      if (curConstraints.state === 0) {
        // can discard any variable constraints now
        curConstraints.variables = [];
      }
    } else {
      curConstraints.variables.push(this.state);
    }
    return;
  }

  visitReferenceType(t: BaseIR): boolean {
    const origState = this.state;
    const typeName = t.name;
    t.typeArgs.forEach((arg, idx) => {
      const curVar = `${typeName}#${idx}`;
      this.state = applyVariableToState(origState, curVar);
      this.traverseType(arg);
    });
    this.state = origState;
    return true;
  }

  visitSig(_name: string, _isStatic: boolean, sig: SigIR): boolean {
    const origState = this.state;
    this.state = reverseVarianceState(origState);
    for (const { type } of sig.params) {
      this.traverseType(type);
    }
    if (sig.spreadParam) {
      this.traverseType(sig.spreadParam.type);
    }
    for (const { type } of sig.kwparams || []) {
      this.traverseType(type);
    }
    this.state = origState;
    this.traverseType(sig.returns);
    return true;
  }
}

function interfaceVarianceConstraints(cls: InterfaceIR): VarianceConstraint[] {
  if (cls.typeParams.length === 0) {
    return [];
  }
  const visitor = new VarianceVisitor(cls);
  visitor.traverseInterface(cls);
  return visitor.constraints;
}

const ASSUMED_CONTRAVARIANT = [
  "ReadableStreamReader",
  "ReadableStreamController",
];
const ASSUMED_COVARIANT = [
  "Promise",
  "PyIterator",
  "IterableIterator_iface",
  "Iterable_iface",
];
const ASSUMED_INVARIANT = ["ReadableStreamReadResult"];

export function calculateInterfaceTypeParamVariances(
  nameToCls: Map<string, InterfaceIR>,
): void {
  type VisitInfo = { begun?: boolean; finished?: boolean };
  const constraintMap = new Map<
    InterfaceIR,
    (VarianceConstraint & VisitInfo)[]
  >();
  for (let cls of nameToCls.values()) {
    constraintMap.set(cls, interfaceVarianceConstraints(cls));
  }

  for (let cls of nameToCls.values()) {
    const constraints = constraintMap.get(cls);
    constraints.forEach((constraint, idx) => {
      visitConstraint(cls, constraint, idx);
      // console.log(cls.name, typeVar.name, state, variables, typeVar.variance);
    });
  }

  function visitConstraint(
    cls: InterfaceIR,
    constraint: VarianceConstraint & VisitInfo,
    idx: number,
  ) {
    let { state, variables } = constraint;
    if (constraint.finished) {
      return;
    }
    try {
      const typeVar = cls.typeParams[idx];
      if (constraint.begun) {
        // A cycle, give up and mark this as invariant...
        console.log("Cycle, marking invariant", cls.name, idx);
        typeVar.variance = Variance.invar;
        return;
      }
      constraint.begun = true;
      if (state === 0 || variables.length === 0) {
        typeVar.variance = intersectVarianceToCircVariance(state);
        return;
      }
      outer: for (const curVariable of variables) {
        let circVariance = curVariable.state;
        for (const [typeIndexPair, parity] of curVariable.variables) {
          if (parity === 0) {
            throw new Error("Not implemented");
          }
          const [targetName, targetIndexString] = typeIndexPair.split("#");
          const targetIndex = Number(targetIndexString);
          if (targetName === cls.name && targetIndex === idx) {
            // Self reference, skip it (this is not 100% correct, for instance
            // if the state is otherwise contravariant)
            continue outer;
          }
          const targetCls = nameToCls.get(targetName);
          const targetConstraint = constraintMap.get(targetCls)?.[targetIndex];
          if (ASSUMED_COVARIANT.includes(targetName)) {
            continue;
          }
          if (ASSUMED_CONTRAVARIANT.includes(targetName)) {
            circVariance *= -1;
            continue;
          }
          if (ASSUMED_INVARIANT.includes(targetName)) {
            circVariance *= 0;
            continue;
          }
          if (targetCls === undefined || targetConstraint === undefined) {
            throw new Error(
              `Missing variance info ${targetName}, ${targetIndex}`,
            );
          }
          visitConstraint(targetCls, targetConstraint, targetIndex);
          circVariance *= targetCls.typeParams[targetIndex].variance;
        }
        state &= circVarianceToIntersectVariance(circVariance);
      }
      typeVar.variance = intersectVarianceToCircVariance(state);
    } finally {
      constraint.finished = true;
    }
  }
}

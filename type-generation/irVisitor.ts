import {
  DeclarationIR,
  TypeAliasIR,
  InterfaceIR,
  PropertyIR,
  BaseIR,
  CallableIR,
  SigIR,
  ArrayTypeIR,
  NumberTypeIR,
  TypeOperatorTypeIR,
  OtherTypeIR,
  ParameterReferenceTypeIR,
  ReferenceTypeIR,
  SimpleTypeIR,
  TupleTypeIR,
  UnionTypeIR,
  TopLevelIR,
  TypeIR,
} from "./astToIR";
import { assertUnreachable } from "./astUtils";

export interface IRVisitorIface {
  visitDeclaration?: (t: DeclarationIR) => boolean;
  visitTypeAlias?: (t: TypeAliasIR) => boolean;
  visitInterface?: (t: InterfaceIR) => boolean;
  visitProperty?: (t: PropertyIR) => boolean;

  visitBase?: (t: BaseIR) => boolean;
  visitCallable?: (t: CallableIR) => boolean;
  visitSig?: (name: string, isStatic: boolean, sig: SigIR) => boolean;

  visitArrayType?: (t: ArrayTypeIR) => boolean;
  visitNumberType?: (t: NumberTypeIR) => boolean;
  visitOperatorType?: (t: TypeOperatorTypeIR) => boolean;
  visitOtherType?: (t: OtherTypeIR) => boolean;
  visitParameterReferenceType?: (t: ParameterReferenceTypeIR) => boolean;
  visitReferenceType?: (t: ReferenceTypeIR) => boolean;
  visitSimpleType?: (t: SimpleTypeIR) => boolean;
  visitTupleType?: (t: TupleTypeIR) => boolean;
  visitUnionType?: (t: UnionTypeIR) => boolean;
}
type IRVisitorType = IRVisitorBase & IRVisitorIface;
// DeclarationIR | InterfaceIR | TypeAliasIR

export class IRVisitorBase {
  traverseTopLevel(this: IRVisitorType, t: TopLevelIR): void {
    switch (t.kind) {
      case "callable":
        this.traverseCallable(t);
        return;
      case "declaration":
        this?.visitDeclaration(t) || this.traverseType(t.type);
        return;
      case "interface":
        this.traverseInterface(t);
        return;
      case "typeAlias":
        this?.visitTypeAlias(t) || this.traverseType(t.type);
        return;
    }
    assertUnreachable(t);
  }

  traverseInterface(this: IRVisitorType, t: InterfaceIR): void {
    if (this.visitInterface?.(t)) {
      return;
    }
    t.bases.forEach((t) => this.traverseBase(t));
    t.methods.forEach((t) => this.traverseCallable(t));
    t.properties.forEach((t) => this.traverseProperty(t));
  }

  traverseProperty(this: IRVisitorType, t: PropertyIR): void {
    if (this.visitProperty?.(t)) {
      return;
    }
    this.traverseType(t.type);
  }

  traverseBase(this: IRVisitorType, t: BaseIR): void {
    if (this.visitBase?.(t)) {
      return;
    }
    t.typeArgs.forEach((t) => this.traverseType(t));
  }

  traverseCallable(this: IRVisitorType, t: CallableIR): void {
    if (this.visitCallable?.(t)) {
      return;
    }
    const { name, isStatic, signatures } = t;
    for (const sig of signatures) {
      this.traverseSig(name, isStatic, sig);
    }
  }

  traverseSig(
    this: IRVisitorType,
    name: string,
    isStatic: boolean,
    sig: SigIR,
  ): void {
    this.visitSig(name, isStatic, sig);
    if (this?.visitSig(name, isStatic, sig)) {
      return;
    }
    sig.params.forEach((p) => this.traverseType(p.type));
    if (sig.spreadParam) {
      this.traverseType(sig.spreadParam.type);
    }
    sig.kwparams.forEach((p) => this.traverseType(p.type));
    this.traverseType(sig.returns);
  }

  traverseType(this: IRVisitorType, t: TypeIR): void {
    switch (t.kind) {
      case "array":
        this.visitArrayType?.(t) || this.traverseType(t.type);
        return;
      case "callable":
        this.traverseCallable(t);
        return;
      case "number":
        this.visitNumberType?.(t);
        return;
      case "operator":
        this.visitOperatorType?.(t) || this.traverseType(t.type);
        return;
      case "other":
        this.visitOtherType?.(t);
        return;
      case "parameterReference":
        this.visitParameterReferenceType?.(t);
        return;
      case "paren":
        this.traverseType(t.type);
        return;
      case "reference":
        this.visitReferenceType?.(t) ||
          t.typeArgs.forEach((t) => this.traverseType(t));
        return;
      case "simple":
        this?.visitSimpleType?.(t);
        return;
      case "tuple":
        this?.visitTupleType?.(t) ||
          t.types.forEach((t) => this.traverseType(t));
        return;
      case "union":
        this?.visitUnionType?.(t) ||
          t.types.forEach((t) => this.traverseType(t));
        return;
    }
    assertUnreachable(t);
  }
}

import { SourceFile } from "ts-morph";
import {
  PyParam,
  PySig,
  renderInnerSignature,
  renderProperty,
  renderPyClass,
  renderSignature,
  renderSignatureGroup,
  renderSimpleDeclaration,
} from "./render.ts";
import { PyClass } from "./types.ts";
import {
  PRELUDE,
  getExtraBases,
  typeReferenceSubsitutions,
} from "./adjustments.ts";

import { PyOther, PyTopLevel, Variance, reverseVariance } from "./types.ts";
import { assertUnreachable } from "./astUtils.ts";
import {
  BaseIR,
  InterfaceIR,
  ParamIR,
  PropertyIR,
  CallableIR,
  SigIR,
  TopLevelIR,
  TypeIR,
  convertFiles,
  ConversionResult,
} from "./astToIR.ts";

function pyClass(name: string, supers: string[], body: string): PyClass {
  const superStems = supers
    .map((sup) => sup.split("[")[0])
    .filter((x) => x !== "Generic");
  return {
    kind: "class",
    name,
    superStems,
    supers,
    body,
  };
}

function pyOther(text: string): PyOther {
  return {
    kind: "other",
    text,
  };
}

function topologicalSortClasses(
  nameToCls: Map<string, InterfaceIR>,
): InterfaceIR[] {
  type AnotatedClass = InterfaceIR & { sorted?: boolean; visited?: boolean };
  const result: InterfaceIR[] = [];
  function visit(cls: AnotatedClass) {
    if (cls.sorted) {
      return;
    }
    if (cls.visited) {
      throw new Error("Cycle");
    }
    cls.visited = true;
    for (const { name } of cls.bases) {
      const superClass = nameToCls.get(name);
      if (!superClass) {
        throw new Error(`Unknown super: ${cls.name} < ${name}`);
      }
      visit(superClass);
    }
    cls.sorted = true;
    result.push(cls);
  }
  for (const cls of nameToCls.values()) {
    visit(cls);
  }
  return result;
}

function fixupClassBases(unsortedClasses: InterfaceIR[]): void {
  const nameToCls = new Map(unsortedClasses.map((cls) => [cls.name, cls]));
  if (nameToCls.size < unsortedClasses.length) {
    throw new Error("Duplicate");
  }
  const classes = topologicalSortClasses(nameToCls);
  const classNameToIndex = new Map(classes.map((cls, idx) => [cls.name, idx]));
  for (const cls of classes) {
    cls.extraBases = getExtraBases(cls.name);
    if (cls.extraBases.length > 0) {
      cls.concrete = true;
    } else {
      for (const { name: sName } of cls.bases) {
        const s = nameToCls.get(sName);
        if (s.concrete) {
          cls.concrete = true;
          break;
        }
      }
    }
    if (cls.name.endsWith("_iface") && !cls.concrete) {
      cls.extraBases.push("Protocol");
    }
    if (!cls.name.endsWith("_iface")) {
      cls.extraBases.push("_JsObject");
    }
    cls.bases.sort(({ name: a }, { name: b }) => {
      return classNameToIndex.get(b) - classNameToIndex.get(a);
    });
  }
}

export function emitFiles(files: SourceFile[]): string[] {
  const result = convertFiles(files);
  return emitIR(result);
}

export function emitIR({ topLevels, typeParams }: ConversionResult): string[] {
  const unsortedClasses = topLevels.filter(
    (x): x is InterfaceIR => x.kind === "interface",
  );
  fixupClassBases(unsortedClasses);
  const pyTopLevels = topLevels.map((e) => renderTopLevelIR(e));
  const typevarDecls = Array.from(
    typeParams,
    (x) => `${x} = TypeVar("${x}")`,
  ).join("\n");
  const output = [PRELUDE, typevarDecls];
  for (const topLevel of pyTopLevels) {
    switch (topLevel.kind) {
      case "class":
        output.push(renderPyClass(topLevel));
        break;

      case "other":
        const { text } = topLevel;
        output.push(text);
        break;
    }
  }
  return output;
}

export function renderTopLevelIR(toplevel: TopLevelIR): PyTopLevel {
  if (toplevel.kind === "declaration") {
    const { name, type } = toplevel;
    const typeStr = renderTypeIR(type, false, Variance.covar);
    return pyOther(renderSimpleDeclaration(name, typeStr));
  }
  if (toplevel.kind === "typeAlias") {
    const { name, type } = toplevel;
    const typeStr = renderTypeIR(type, false, Variance.covar);
    return pyOther(`${name} = ${typeStr}`);
  }
  if (toplevel.kind === "interface") {
    return renderInterface(toplevel);
  }
  if (toplevel.kind === "callable") {
    return pyOther(renderSignatureGroup2(toplevel, false).join("\n"));
  }
  assertUnreachable(toplevel);
}

function renderIRSignatures(
  irSigs: readonly SigIR[],
  variance: Variance,
  topLevelName?: string,
): string {
  const pySigs = irSigs.map((sig) => renderSig(sig, variance));
  if (!topLevelName) {
    const converted = pySigs.map(renderInnerSignature);
    return converted.join(" | ");
  }
  const converted = pySigs.map((sig) => renderSignature(topLevelName, sig));
  if (converted.length === 1) {
    return converted[0];
  }
  return converted.map((x) => "@overload\n" + x).join("\n\n");
}

function renderSig(
  sig: SigIR,
  variance: Variance,
  decorators: string[] = [],
  isStatic: boolean = false,
): PySig {
  const renderParam = ({ name, optional, type }: ParamIR): PyParam => {
    const pyType = renderTypeIR(type, optional, paramVariance);
    return { name, optional, pyType };
  };
  const {
    params: origParams,
    spreadParam: origSpreadParam,
    kwparams: origKwparams,
    returns: origReturns,
  } = sig;
  const paramVariance = reverseVariance(variance);
  const params = origParams.map(renderParam);
  const kwparams = origKwparams?.map(renderParam);
  const spreadParam = origSpreadParam
    ? renderParam(origSpreadParam)
    : undefined;
  const returns = renderTypeIR(origReturns, false, variance);
  if (isStatic) {
    decorators.push("classmethod");
  }
  return { params, spreadParam, kwparams, returns, decorators };
}

export function renderSignatureGroup2(
  { name, signatures: sigs, isStatic }: CallableIR,
  isMethod: boolean,
): string[] {
  const pySigs = sigs.map((sig) =>
    renderSig(sig, Variance.covar, [], isStatic),
  );
  return renderSignatureGroup({ name, sigs: pySigs }, isMethod);
}

export function renderBase({ name, typeParams }: BaseIR): string {
  if (typeParams.length > 0) {
    const joined = typeParams
      .map((t) => renderTypeIR(t, false, Variance.covar))
      .join(", ");
    name += `[${joined}]`;
  }
  return name;
}

function renderInterface({
  name,
  properties,
  methods,
  typeParams,
  bases,
  extraBases,
}: InterfaceIR): PyClass {
  const entries = ([] as string[]).concat(
    properties.map((prop) => renderProperty2(prop)),
    methods.flatMap((gp) => renderSignatureGroup2(gp, true)),
  );
  const newSupers = bases.map((b) => renderBase(b));
  if (typeParams.length > 0) {
    const joined = typeParams.join(", ");
    newSupers.push(`Generic[${joined}]`);
  }
  extraBases ??= [];
  newSupers.push(...extraBases);
  return pyClass(name, newSupers, entries.join("\n"));
}

export function renderProperty2(property: PropertyIR): string {
  const { isOptional, name, type, isReadonly, isStatic } = property;
  const pyType = renderTypeIR(type, isOptional, Variance.covar, name);
  return renderProperty(name, pyType, isReadonly, isStatic);
}

export function renderTypeIR(
  ir: TypeIR,
  isOptional: boolean,
  variance: Variance,
  topLevelName?: string,
): string {
  if (isOptional) {
    topLevelName = undefined;
  }
  let result = renderTypeIRInner(ir, variance, topLevelName);
  if (!isOptional) {
    return result;
  }
  if (
    (ir.kind === "simple" && ir.text === "None") ||
    (ir.kind === "union" &&
      ir.types.some((x) => x.kind === "simple" && x.text === "None"))
  ) {
    return result;
  }
  result += " | None";
  return result;
}

function renderTypeIRInner(
  ir: TypeIR,
  variance: Variance,
  topLevelName?: string,
): string {
  if (ir.kind === "simple") {
    return ir.text;
  }
  if (ir.kind === "union") {
    return ir.types.map((ty) => renderTypeIR(ty, false, variance)).join(" | ");
  }
  if (ir.kind === "paren") {
    const inner = renderTypeIR(ir.type, false, variance);
    return `(${inner})`;
  }
  if (ir.kind === "array") {
    const eltType = renderTypeIR(ir.type, false, variance);
    if (variance === Variance.contra) {
      return `PyMutableSequence[${eltType}]`;
    }
    return `JsArray[${eltType}]`;
  }
  if (ir.kind == "tuple") {
    let elts = ir.types
      .map((elt) => renderTypeIR(elt, false, variance))
      .join(", ");
    if (elts === "") {
      elts = "()";
    }
    return `tuple[${elts}]`;
  }
  if (ir.kind === "operator") {
    // Ignore type operators
    return renderTypeIR(ir.type, false, variance);
  }
  if (ir.kind === "callable") {
    return renderIRSignatures(ir.signatures, variance, topLevelName);
  }
  if (ir.kind === "other") {
    return "Any";
  }
  if (ir.kind === "parameterReference") {
    return ir.name;
  }
  if (ir.kind === "reference") {
    let { identName: name, typeArgs } = ir;
    const res = typeReferenceSubsitutions(name, typeArgs, variance);
    if (res) {
      return res;
    }
    const args = typeArgs.map((ty) => renderTypeIR(ty, false, Variance.none));
    let fmtArgs = "";
    if (args.length) {
      fmtArgs = `[${args.join(", ")}]`;
    }
    return `${name}${fmtArgs}`;
  }
  assertUnreachable(ir);
}

import { SourceFile } from "ts-morph";
import {
  PyParam,
  PySig,
  renderInnerSignature,
  renderProperty,
  renderPyClass,
  renderSignature,
  renderSimpleDeclaration,
  uniqBy,
} from "./render.ts";
import {
  PRELUDE,
  adjustFunction,
  adjustInterfaceIR,
  getExtraBases,
  typeReferenceSubsitutions,
} from "./adjustments.ts";

import { Variance, reverseVariance } from "./types.ts";
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
  for (let cls of unsortedClasses) {
    adjustInterfaceIR(cls);
  }
  for (let obj of topLevels) {
    if (obj.kind === "callable") {
      adjustFunction(obj);
    }
    if (obj.kind === "interface") {
      obj.methods.forEach(adjustFunction);
    }
  }
  const typevarDecls = Array.from(
    typeParams,
    (x) => `${x} = TypeVar("${x}")`,
  ).join("\n");
  const rendered = topLevels.map((e) => renderTopLevelIR(e));
  return [PRELUDE, typevarDecls, ...rendered];
}

export function renderTopLevelIR(toplevel: TopLevelIR): string {
  if (toplevel.kind === "declaration") {
    const { name, type } = toplevel;
    const typeStr = renderTypeIR(type);
    return renderSimpleDeclaration(name, typeStr);
  }
  if (toplevel.kind === "typeAlias") {
    const { name, type } = toplevel;
    const typeStr = renderTypeIR(type);
    return `${name} = ${typeStr}`;
  }
  if (toplevel.kind === "interface") {
    return renderInterface(toplevel);
  }
  if (toplevel.kind === "callable") {
    return renderSignatureGroup(toplevel, false).join("\n");
  }
  assertUnreachable(toplevel);
}

function renderIRSignatures(
  irSigs: readonly SigIR[],
  settings: {
    variance?: Variance;
    topLevelName?: string;
    numberType?: string;
  },
): string {
  const topLevelName = settings.topLevelName;
  const settings2 = Object.assign(settings, { isStatic: false });
  const pySigs = irSigs.map((sig) => renderSig(sig, settings2));
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
  {
    variance = Variance.covar,
    numberType,
    isStatic,
  }: {
    variance?: Variance;
    topLevelName?: string;
    numberType?: string;
    isStatic: boolean;
  },
): PySig {
  const renderParam = ({ name, isOptional, type }: ParamIR): PyParam => {
    const pyType = renderTypeIR(type, {
      isOptional,
      variance: paramVariance,
      numberType,
    });
    return { name, isOptional, pyType };
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
  const returns = renderTypeIR(origReturns, { variance, numberType });
  const decorators = isStatic ? ["classmethod"] : [];
  return { params, spreadParam, kwparams, returns, decorators };
}

export function renderSignatureGroup(
  { name, signatures: sigs, isStatic }: CallableIR,
  isMethod: boolean,
  numberType?: string,
): string[] {
  const uniqueSigs = uniqBy(sigs, (sig) => {
    sig = structuredClone(sig);
    // Remove parameter names to perform comparison so if two sigs only differ
    // in param names they should compare equal.
    // TODO: prune sigs more aggressively?
    // TODO: move this out of the render stage into the transform stage
    const deleteName = (param) => delete param["name"];
    sig.params.map(deleteName);
    sig.kwparams?.map(deleteName);
    delete sig?.spreadParam?.name;
    return JSON.stringify(sig);
  });
  const pySigs = uniqueSigs.map((sig) =>
    renderSig(sig, { isStatic, numberType }),
  );
  const decorators: string[] = [];
  if (uniqueSigs.length > 1) {
    decorators.push("overload");
  }
  return pySigs.map((sig) => renderSignature(name, sig, decorators, isMethod));
}

export function renderBase({ name, typeParams }: BaseIR): string {
  if (typeParams.length > 0) {
    const joined = typeParams.map((t) => renderTypeIR(t)).join(", ");
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
  numberType,
}: InterfaceIR): string {
  const entries = ([] as string[]).concat(
    properties.map((prop) => renderProperty2(prop, numberType)),
    methods.flatMap((gp) => renderSignatureGroup(gp, true, numberType)),
  );
  const newSupers = bases.map((b) => renderBase(b));
  if (typeParams.length > 0) {
    const joined = typeParams.join(", ");
    newSupers.push(`Generic[${joined}]`);
  }
  extraBases ??= [];
  newSupers.push(...extraBases);
  return renderPyClass(name, newSupers, entries.join("\n"));
}

export function renderProperty2(
  property: PropertyIR,
  numberType?: string,
): string {
  const { isOptional, name, type, isReadonly, isStatic } = property;
  const pyType = renderTypeIR(type, {
    isOptional,
    topLevelName: name,
    numberType,
  });
  return renderProperty(name, pyType, isReadonly, isStatic);
}

type RenderTypeSettings = {
  isOptional?: boolean;
  variance?: Variance;
  topLevelName?: string;
  numberType?: string;
};

export function renderTypeIR(
  ir: TypeIR,
  settings: RenderTypeSettings = {},
): string {
  settings.variance ??= Variance.covar;
  let { isOptional } = settings;
  if (isOptional) {
    settings.topLevelName = undefined;
    settings.isOptional = false;
  }
  let result = renderTypeIRInner(ir, settings);
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

function renderTypeIRInner(ir: TypeIR, settings: RenderTypeSettings): string {
  const { variance, numberType, topLevelName } = settings;
  settings.topLevelName = undefined;
  if (ir.kind === "simple") {
    return ir.text;
  }
  if (ir.kind === "union") {
    return ir.types.map((ty) => renderTypeIR(ty, settings)).join(" | ");
  }
  if (ir.kind === "paren") {
    const inner = renderTypeIR(ir.type, settings);
    return `(${inner})`;
  }
  if (ir.kind === "array") {
    const eltType = renderTypeIR(ir.type, settings);
    if (variance === Variance.contra) {
      return `PyMutableSequence[${eltType}]`;
    }
    return `JsArray[${eltType}]`;
  }
  if (ir.kind == "tuple") {
    let elts = ir.types.map((elt) => renderTypeIR(elt, settings)).join(", ");
    if (elts === "") {
      elts = "()";
    }
    return `tuple[${elts}]`;
  }
  if (ir.kind === "operator") {
    // Ignore type operators
    return renderTypeIR(ir.type, settings);
  }
  if (ir.kind === "callable") {
    return renderIRSignatures(ir.signatures, {
      variance,
      topLevelName,
      numberType,
    });
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
    const args = typeArgs.map((ty) =>
      renderTypeIR(ty, { variance: Variance.none, numberType }),
    );
    let fmtArgs = "";
    if (args.length) {
      fmtArgs = `[${args.join(", ")}]`;
    }
    return `${name}${fmtArgs}`;
  }
  if (ir.kind === "number") {
    return numberType || "int | float";
  }
  assertUnreachable(ir);
}

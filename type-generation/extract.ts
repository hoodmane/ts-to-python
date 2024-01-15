import { SourceFile } from "ts-morph";
import {
  PyParam,
  PySig,
  indent,
  renderInnerSignature,
  renderProperty,
  renderSignature,
  renderSimpleDeclaration,
  uniqBy,
} from "./render.ts";
import {
  CLASS_TYPE_IGNORES,
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
    return renderCallableIR(toplevel, false).join("\n");
  }
  assertUnreachable(toplevel);
}

export function renderCallableIR(
  { name, signatures: sigs, isStatic }: CallableIR,
  isMethod: boolean,
  numberType?: string,
): string[] {
  const decorators = isStatic ? ["classmethod"] : [];
  return renderIRSigs(sigs, {
    topLevelName: name,
    numberType,
    isMethod,
    decorators,
  });
}

function renderIRSigs(
  irSigs: readonly SigIR[],
  settings: {
    variance?: Variance;
    topLevelName?: string;
    numberType?: string;
    isMethod?: boolean;
    decorators?: string[];
  },
): string[] {
  irSigs = uniqBy(irSigs, (sig) => {
    // Remove parameter names to perform comparison so if two sigs only differ
    // in param names they should compare equal.
    // TODO: prune sigs more aggressively?
    // TODO: move this out of the render stage into the transform stage
    return JSON.stringify(sig, (key, value) =>
      key !== "name" ? value : undefined,
    );
  });
  if (irSigs.length > 1) {
    settings = structuredClone(settings);
    settings.decorators.push("overload");
  }
  return irSigs.map((sig) => renderSig(sig, settings));
}

function irSigToPySig(
  sig: SigIR,
  {
    variance = Variance.covar,
    numberType,
  }: {
    variance?: Variance;
    numberType?: string;
  },
): PySig {
  const paramVariance = reverseVariance(variance);
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
  const params = origParams.map(renderParam);
  const kwparams = origKwparams?.map(renderParam);
  const spreadParam = origSpreadParam
    ? renderParam(origSpreadParam)
    : undefined;
  const returns = renderTypeIR(origReturns, { variance, numberType });
  return { params, spreadParam, kwparams, returns };
}

function renderSig(
  irSig: SigIR,
  {
    variance = Variance.covar,
    numberType,
    topLevelName,
    decorators = [],
    isMethod,
  }: {
    variance?: Variance;
    topLevelName?: string;
    numberType?: string;
    decorators?: string[];
    isMethod?: boolean;
  },
): string {
  const pySig = irSigToPySig(irSig, { variance, numberType });
  if (topLevelName) {
    return renderSignature(topLevelName, pySig, decorators, isMethod);
  }
  // TODO: consider warning here if interesting values are provided for the
  // stuff we're ignoring...
  return renderInnerSignature(pySig);
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
  bases: irBases,
  extraBases = [],
  numberType,
}: InterfaceIR): string {
  const entries = ([] as string[]).concat(
    properties.map((prop) => renderProperty(prop, numberType)),
    methods.flatMap((gp) => renderCallableIR(gp, true, numberType)),
  );
  const bases = irBases.map((b) => renderBase(b));
  if (typeParams.length > 0) {
    const joined = typeParams.join(", ");
    bases.push(`Generic[${joined}]`);
  }
  bases.push(...extraBases);
  let body = entries.join("\n");
  if (body.trim() === "") {
    body = "pass";
  }
  body = indent(body, " ".repeat(4));
  let basesString = "";
  if (bases.length > 0) {
    basesString = `(${bases.join(", ")})`;
  }
  return `class ${name}${basesString}:${CLASS_TYPE_IGNORES}\n${body}`;
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
  settings = Object.assign({}, settings);
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
    return renderIRSigs(ir.signatures, {
      variance,
      topLevelName,
      numberType,
    }).join("\n");
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

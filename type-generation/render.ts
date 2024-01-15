import {
  CLASS_TYPE_IGNORES,
  METHOD_TYPE_IGNORES,
  PROPERTY_TYPE_IGNORES,
  typeReferenceSubsitutions,
} from "./adjustments.ts";
import {
  BaseIR,
  CallableIR,
  InterfaceIR,
  ParamIR,
  PropertyIR,
  SigIR,
  TopLevelIR,
  TypeIR,
} from "./astToIR.ts";
import { assertUnreachable } from "./astUtils.ts";
import { Variance, reverseVariance } from "./types.ts";

//
// Utilities
//

export function uniqBy<T, S>(l: readonly T[], key: (k: T) => S) {
  const seen = new Set();
  return l.filter(function (item) {
    const k = key(item);
    const result = !seen.has(k);
    seen.add(k);
    return result;
  });
}

export function indent(x: string, prefix: string): string {
  return x
    .split("\n")
    .map((e) => prefix + e)
    .join("\n");
}

function isIllegal(name) {
  return (
    /["[$]/.test(name) ||
    /^[0-9]/.test(name) ||
    ["addEventListener", "removeEventListener"].includes(name)
  );
}

// prettier-ignore
const pythonReservedWords = new Set([
  "False",  "await", "else",     "import", "pass",   "None",    "break",
  "except", "in",    "raise",    "True",   "class",  "finally", "is",
  "return", "and",   "continue", "for",    "lambda", "try",     "as",
  "def",    "from",  "nonlocal", "while",  "assert", "del",     "global",
  "not",    "with",  "async",    "elif",   "if",     "or",      "yield",
  // extras
  "float",
]);

export function sanitizeReservedWords(name) {
  if (pythonReservedWords.has(name)) {
    name += "_";
  }
  return name;
}

//
// Functions that format strings but don't touch the IR
//

export type PyParam = {
  name: string;
  pyType: string;
  isOptional: boolean;
};
export type PySig = {
  params: PyParam[];
  spreadParam?: PyParam;
  kwparams?: PyParam[];
  returns: string;
};

function renderParam({ name, pyType, isOptional: optional }: PyParam) {
  const maybeDefault = optional ? " = None" : "";
  name = sanitizeReservedWords(name);
  return `${name}: ${pyType}${maybeDefault}`;
}

export function renderInnerSignature(sig: PySig): string {
  const paramTypes = sig.params.map(({ pyType }) => pyType);
  return `Callable[[${paramTypes.join(", ")}], ${sig.returns}]`;
}

export function renderSignature(
  name: string,
  sig: PySig,
  decorators: string[] = [],
  isMethod: boolean = true,
): string {
  if (isIllegal(name)) {
    return "";
  }
  name = sanitizeReservedWords(name);
  const formattedParams = sig.params.map(renderParam);
  if (isMethod) {
    formattedParams.unshift("self");
  }
  if (formattedParams.length > 0) {
    formattedParams.push("/");
  }
  if (sig.spreadParam) {
    const { name, pyType } = sig.spreadParam;
    formattedParams.push(`*${name}: ${pyType}`);
  }
  if (sig.kwparams?.length > 0) {
    formattedParams.push("*");
    formattedParams.push(...sig.kwparams.map(renderParam));
  }
  const joinedParams = formattedParams.join(", ");
  const decs = decorators.map((x) => "@" + x + "\n").join("");
  return (
    `${decs}def ${name}(${joinedParams}) -> ${sig.returns}: ...` +
    METHOD_TYPE_IGNORES
  );
}

export function renderSimpleDeclaration(name: string, type: string) {
  return `${name}: ${type} = ...` + PROPERTY_TYPE_IGNORES;
}

//
// Functions that format IR into strings
//

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

function renderInterface({
  name,
  properties,
  methods,
  typeParams,
  bases: irBases,
  extraBases = [],
  numberType,
}: InterfaceIR): string {
  const bases = irBases.map((b) => renderBase(b));
  if (typeParams.length > 0) {
    const joined = typeParams.join(", ");
    bases.push(`Generic[${joined}]`);
  }
  bases.push(...extraBases);
  let basesString = "";
  if (bases.length > 0) {
    basesString = `(${bases.join(", ")})`;
  }

  const entries = ([] as string[]).concat(
    properties.map((prop) => renderProperty(prop, numberType)),
    methods.flatMap((gp) => renderCallableIR(gp, true, numberType)),
  );
  let body = entries.join("\n");
  if (body.trim() === "") {
    body = "pass";
  }
  body = indent(body, " ".repeat(4));

  return `class ${name}${basesString}:${CLASS_TYPE_IGNORES}\n${body}`;
}

export function renderBase({ name, typeParams }: BaseIR): string {
  if (typeParams.length > 0) {
    const joined = typeParams.map((t) => renderTypeIR(t)).join(", ");
    name += `[${joined}]`;
  }
  return name;
}

export function renderProperty(
  property: PropertyIR,
  numberType?: string,
): string {
  let { isOptional, name, type: typeIR, isReadonly, isStatic } = property;
  let typeString = renderTypeIR(typeIR, {
    isOptional,
    topLevelName: name,
    numberType,
  });
  if (isIllegal(name)) {
    return "";
  }
  name = sanitizeReservedWords(name);
  const isDef = typeString.includes("def");
  if (!isDef && isReadonly && !isStatic) {
    const decs = ["property"];
    return renderSignature(name, { params: [], returns: typeString }, decs);
  }
  if (isDef) {
    if (isStatic) {
      typeString = "@classmethod\n" + typeString;
    }
    return typeString;
  }
  if (isStatic) {
    typeString = `ClassVar[${typeString}]`;
  }
  return renderSimpleDeclaration(name, typeString);
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

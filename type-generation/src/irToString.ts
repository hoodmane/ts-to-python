import {
  CLASS_TYPE_IGNORES,
  METHOD_TYPE_IGNORES,
  PROPERTY_TYPE_IGNORES,
  typeReferenceSubsitutions,
} from "./adjustments.ts";
import {
  CallableIR,
  DeclarationIR,
  InterfaceIR,
  ParamIR,
  PropertyIR,
  ReferenceTypeIR,
  SigIR,
  TypeAliasIR,
  TypeIR,
  TypeParamIR,
} from "./ir.ts";
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

function isIllegal(name: string): boolean {
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

export function sanitizeReservedWords(name: string): string {
  if (pythonReservedWords.has(name)) {
    name += "_";
  }
  return name;
}

//
// Functions that format strings but don't touch the IR
//

/**
 * Signatures need to be represented in two different ways. Suppose we have the
 * following:
 *
 * ```
 * function f(a: string): void;
 * function f(a: string, b: string): void;
 * ```
 *
 * 1. in a context where a type is needed, it should be converted to
 *    Callable[[str], None] | Callable[[str, str], None]
 *
 * 2. in a context where a declaration is needed, it should be converted to:
 *
 *    ```
 *    @overload
 *    def f(a: str) -> None: ...
 *    @overload
 *    def f(a: str, b: str) -> None: ...
 *    ```
 *
 * We use PySig to store the data of the signature until we pick the
 * representation.
 *
 * TODO: Ideally we should add a third representation: if we need to represent
 * `(a?: string): void`, this cannot be written with `Callable` it must instead
 * be declared like:
 * ```
 * class FuncType7(Protocol):
 *    def __call__(self, a: str = ""): ...
 * ```
 */
type PySig = {
  params: PyParam[];
  spreadParam?: PyParam;
  kwparams?: PyParam[];
  returns: string;
};

type PyParam = {
  name: string;
  pyType: string;
  isOptional: boolean;
};

/**
 * Convert a PySig to a type.
 *
 * Ignores optional params, spreadParam, and kwparams.
 * TODO: handle these
 */
function pySigToTypeString(sig: PySig): string {
  const paramTypes = sig.params.map(({ pyType }) => pyType);
  return `Callable[[${paramTypes.join(", ")}], ${sig.returns}]`;
}

function pyParamToString({
  name,
  pyType,
  isOptional: optional,
}: PyParam): string {
  const maybeDefault = optional ? " = None" : "";
  name = sanitizeReservedWords(name);
  return `${name}: ${pyType}${maybeDefault}`;
}

function typeParamToString({ name, spread }: TypeParamIR): string {
  let prefix = "";
  if (spread) {
    prefix = "*";
  }
  return prefix + name;
}

function pySigToDeclarationString(
  name: string,
  sig: PySig,
  decorators: string[] = [],
  isMethod: boolean = true,
  typeParams?: TypeParamIR[],
): string {
  if (isIllegal(name)) {
    return "";
  }
  name = sanitizeReservedWords(name);

  let typeParamsString = "";
  if (typeParams && typeParams.length > 0) {
    const typeParamsBody = typeParams.map(typeParamToString).join(", ");
    typeParamsString = `[${typeParamsBody}]`;
  }

  const formattedParams = sig.params.map(pyParamToString);
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
  if (sig.kwparams?.length) {
    formattedParams.push("*");
    formattedParams.push(...sig.kwparams.map(pyParamToString));
  }
  const joinedParams = formattedParams.join(", ");
  const decs = decorators.map((x) => "@" + x + "\n").join("");
  return (
    `${decs}def ${name}${typeParamsString}(${joinedParams}) -> ${sig.returns}: ...` +
    METHOD_TYPE_IGNORES
  );
}

function simpleDeclaration(name: string, type: string) {
  return `${name}: ${type} = ...` + PROPERTY_TYPE_IGNORES;
}

//
// Functions that format IR into strings
//

export function declarationIRToString({ name, type }: DeclarationIR): string {
  const typeStr = typeIRToString(type);
  return simpleDeclaration(name, typeStr);
}

export function typeAliasIRToString({
  name,
  type,
  typeParams,
}: TypeAliasIR): string {
  const typeStr = typeIRToString(type);
  let typeParamsString = "";
  if (typeParams.length > 0) {
    typeParamsString = `[${typeParams.map(typeParamToString).join(", ")}]`;
  }

  return `type ${name}${typeParamsString} = ${typeStr}`;
}

export function interfaceIRToString({
  name,
  properties,
  methods,
  typeParams,
  bases: irBases,
  extraBases = [],
  numberType,
}: InterfaceIR): string {
  const bases = irBases.map((b) => baseIRToString(b));
  bases.push(...extraBases);
  let basesString = "";
  if (bases.length > 0) {
    basesString = `(${bases.join(", ")})`;
  }

  let typeParamsString = "";
  if (typeParams.length > 0) {
    typeParamsString = `[${typeParams.map(typeParamToString).join(", ")}]`;
  }

  const entries = ([] as string[]).concat(
    properties.map((prop) => propertyIRToString(prop, numberType)),
    methods.flatMap((gp) => callableIRToString(gp, true, numberType)),
  );
  let body = entries.join("\n");
  if (body.trim() === "") {
    body = "pass";
  }
  body = indent(body, " ".repeat(4));

  return `class ${name}${typeParamsString}${basesString}:${CLASS_TYPE_IGNORES}\n${body}`;
}

export function baseIRToString({ name, typeArgs }: ReferenceTypeIR): string {
  if (typeArgs.length > 0) {
    const joined = typeArgs.map((t) => typeIRToString(t)).join(", ");
    name += `[${joined}]`;
  }
  return name;
}

export function propertyIRToString(
  property: PropertyIR,
  numberType?: string,
): string {
  let { isOptional, name, type: typeIR, isReadonly, isStatic } = property;
  let typeString = typeIRToString(typeIR, {
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
    return pySigToDeclarationString(
      name,
      { params: [], returns: typeString },
      decs,
    );
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
  return simpleDeclaration(name, typeString);
}

export function callableIRToString(
  { name, signatures: sigs, isStatic }: CallableIR,
  isMethod: boolean,
  numberType?: string,
): string[] {
  const decorators = isStatic ? ["classmethod"] : [];
  return sigIRListToString(sigs, {
    topLevelName: name,
    numberType,
    isMethod,
    decorators,
  });
}

function sigIRListToString(
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
    // TODO: move this out of the render stage into the transform stage?
    return JSON.stringify(sig, (key, value) =>
      key !== "name" ? value : undefined,
    );
  });
  if (irSigs.length > 1) {
    settings = structuredClone(settings);
    settings.decorators ??= [];
    settings.decorators.push("overload");
  }
  return irSigs.map((sig) => irSigToString(sig, settings));
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
  const irParamToPyParam = ({ name, isOptional, type }: ParamIR): PyParam => {
    const pyType = typeIRToString(type, {
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
  if (origKwparams) {
    const kwparamsSet = new Set(origKwparams?.map((x) => x.name));
    for (const param of origParams) {
      if (kwparamsSet.has(param.name)) {
        param.name += "_";
      }
    }
  }
  const params = origParams.map(irParamToPyParam);
  const kwparams = origKwparams?.map(irParamToPyParam);
  const spreadParam = origSpreadParam
    ? irParamToPyParam(origSpreadParam)
    : undefined;
  const returns = typeIRToString(origReturns, { variance, numberType });
  return { params, spreadParam, kwparams, returns };
}

function irSigToString(
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
    return pySigToDeclarationString(
      topLevelName,
      pySig,
      decorators,
      isMethod,
      irSig.typeParams,
    );
  }
  // TODO: consider warning here if interesting values are provided for the
  // stuff we're ignoring...
  return pySigToTypeString(pySig);
}

type RenderTypeSettings = {
  isOptional?: boolean;
  variance?: Variance;
  topLevelName?: string;
  numberType?: string;
};

export function typeIRToString(
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
  let result = typeIRToStringHelper(ir, settings);
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

function typeIRToStringHelper(
  ir: TypeIR,
  settings: RenderTypeSettings,
): string {
  const { variance, numberType, topLevelName } = settings;
  settings.topLevelName = undefined;
  if (ir.kind === "simple") {
    return ir.text;
  }
  if (ir.kind === "union") {
    return ir.types.map((ty) => typeIRToString(ty, settings)).join(" | ");
  }
  if (ir.kind === "paren") {
    const inner = typeIRToString(ir.type, settings);
    return `(${inner})`;
  }
  if (ir.kind === "array") {
    const eltType = typeIRToString(ir.type, settings);
    if (variance === Variance.contra) {
      return `PyMutableSequence[${eltType}]`;
    }
    return `JsArray[${eltType}]`;
  }
  if (ir.kind == "tuple") {
    let elts = ir.types.map((elt) => typeIRToString(elt, settings)).join(", ");
    if (elts === "") {
      elts = "()";
    }
    return `tuple[${elts}]`;
  }
  if (ir.kind === "operator") {
    // Ignore type operators
    return typeIRToString(ir.type, settings);
  }
  if (ir.kind === "callable") {
    return sigIRListToString(ir.signatures, {
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
    let { name: name, typeArgs } = ir;
    const res = typeReferenceSubsitutions(name, typeArgs, variance!);
    if (res) {
      return res;
    }
    const args = typeArgs.map((ty) =>
      typeIRToString(ty, { variance: Variance.none, numberType }),
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
  if (ir.kind === "spread") {
    const eltType = typeIRToString(ir.type, settings);
    return `*${eltType}`;
  }
  assertUnreachable(ir);
}

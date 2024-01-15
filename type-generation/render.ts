import {
  METHOD_TYPE_IGNORES,
  PROPERTY_TYPE_IGNORES,
} from "./adjustments.ts";
import { renderTypeIR } from "./extract.ts";
import { PropertyIR } from "./astToIR.ts";

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

const pythonReservedWords = new Set([
  "False",
  "await",
  "else",
  "import",
  "pass",
  "None",
  "break",
  "except",
  "in",
  "raise",
  "True",
  "class",
  "finally",
  "is",
  "return",
  "and",
  "continue",
  "for",
  "lambda",
  "try",
  "as",
  "def",
  "from",
  "nonlocal",
  "while",
  "assert",
  "del",
  "global",
  "not",
  "with",
  "async",
  "elif",
  "if",
  "or",
  "yield",
  // extras
  "float",
]);

export function sanitizeReservedWords(name) {
  if (pythonReservedWords.has(name)) {
    name += "_";
  }
  return name;
}

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
  const decs = decorators
    .map((x) => "@" + x + "\n")
    .join("");
  return (
    `${decs}def ${name}(${joinedParams}) -> ${sig.returns}: ...` +
    METHOD_TYPE_IGNORES
  );
}

export function renderSimpleDeclaration(name: string, type: string) {
  return `${name}: ${type} = ...` + PROPERTY_TYPE_IGNORES;
}

export function renderProperty(
  property: PropertyIR,
  numberType?: string
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

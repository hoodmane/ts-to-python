import {
  CLASS_TYPE_IGNORES,
  METHOD_TYPE_IGNORES,
  PROPERTY_TYPE_IGNORES,
} from "./adjustments.ts";
import { PyClass } from "./types.ts";

export type PyParam = {
  name: string;
  pyType: string;
  optional: boolean;
};
export type PySig = {
  params: PyParam[];
  spreadParam?: PyParam;
  kwparams?: PyParam[];
  returns: string;
  decorators?: string[];
};
export type PySigGroup = { name: string; sigs: PySig[] };

export function uniqBy<T, S>(l: T[], key: (k: T) => S) {
  const seen = new Set();
  return l.filter(function (item) {
    const k = key(item);
    const result = !seen.has(k);
    seen.add(k);
    return result;
  });
}

function indent(x: string, prefix: string): string {
  return x
    .split("\n")
    .map((e) => prefix + e)
    .join("\n");
}

export function renderPyClass({ name, supers, body }: PyClass): string {
  if (body.trim() === "") {
    body = "pass";
  }
  body = indent(body, " ".repeat(4));
  let supersList = "";
  if (supers.length > 0) {
    supersList = `(${supers.join(", ")})`;
  }
  return `class ${name}${supersList}:${CLASS_TYPE_IGNORES}\n${body}`;
}

export function renderSignatureGroup(
  sigGroup: PySigGroup,
  isMethod: boolean,
): string[] {
  const extraDecorators: string[] = [];
  const uniqueSigs = uniqBy(sigGroup.sigs, (sig) => {
    sig = structuredClone(sig);
    sig.params.map((param) => delete param["name"]);
    return JSON.stringify(sig);
  });
  if (uniqueSigs.length > 1) {
    extraDecorators.push("overload");
  }

  return uniqueSigs.map((sig) =>
    renderSignature(sigGroup.name, sig, extraDecorators, isMethod),
  );
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

function renderParam({ name, pyType, optional }: PyParam) {
  const maybeDefault = optional ? " = None" : "";
  name = sanitizeReservedWords(name);
  return `${name}: ${pyType}${maybeDefault}`;
}

export function renderSignature(
  name: string,
  sig: PySig,
  extraDecorators: string[] = [],
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
  formattedParams.push("/");
  if (sig.spreadParam) {
    const { name, pyType } = sig.spreadParam;
    formattedParams.push(`*${name}: ${pyType}`);
  }
  if (sig.kwparams) {
    formattedParams.push("*");
    formattedParams.push(...sig.kwparams.map(renderParam));
  }
  const joinedParams = formattedParams.join(", ");
  const decs = (sig.decorators || [])
    .concat(extraDecorators)
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
  name: string,
  type: string,
  readOnly: boolean,
  isStatic: boolean = false,
): string {
  if (isIllegal(name)) {
    return "";
  }
  name = sanitizeReservedWords(name);
  const isDef = type.includes("def");
  if (!isDef && readOnly && !isStatic) {
    const decs = ["property"];
    return renderSignature(name, { params: [], returns: type }, decs);
  }
  if (isDef) {
    if (isStatic) {
      type = "@classmethod\n" + type;
    }
    return type;
  }
  if (isStatic) {
    type = `ClassVar[${type}]`;
  }
  return renderSimpleDeclaration(name, type);
}

export function renderInnerSignature(sig: PySig): string {
  const paramTypes = sig.params.map(({ pyType }) => pyType);
  return `Callable[[${paramTypes.join(", ")}], ${sig.returns}]`;
}

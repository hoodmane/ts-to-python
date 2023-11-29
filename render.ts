export type PyParam = {
  name: string;
  pyType: string;
  optional: boolean;
  spread?: boolean;
};
export type PySig = {
  params: PyParam[];
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

// ignores: Cannot determine consistent method resolution order (MRO)
const CLASS_TYPE_IGNORES = " # type:ignore[misc,unused-ignore]";
// Ignores:
// [misc]:
//    Overloaded function signature 2 will never be matched: signature 1's parameter type(s) are the same or broader
// [override]:
//    Argument 1 of "someMethod" is incompatible with supertype "superType"
//    Cannot override writeable attribute with read-only property
//    Signature of "someMethod" incompatible with supertype "superType"
// [overload-overlap]:
//    Overloaded function signatures 1 and 6 overlap with incompatible return types
// [unused-ignore]:
// [type-arg]:
//    Missing type parameters for generic type "?" (could be fixed by tracking type parameter defaults)
let METHOD_TYPE_IGNORES =
  " # type:ignore[override,overload-overlap,misc,type-arg,unused-ignore]";
// TYPE_IGNORES = "";
let PROPERTY_TYPE_IGNORES = " # type:ignore[type-arg,unused-ignore]";

export function renderPyClass(
  name: string,
  supers: string[],
  body: string,
): string {
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

export function renderSignatureGroup(sigGroup: PySigGroup): string[] {
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
    renderSignature(sigGroup.name, sig, extraDecorators),
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

export function renderSignature(
  name: string,
  sig: PySig,
  extraDecorators: string[] = [],
): string {
  if (isIllegal(name)) {
    return "";
  }
  name = sanitizeReservedWords(name);
  const formattedParams = sig.params.map(
    ({ name, pyType, optional, spread }) => {
      const maybeDefault = optional ? " = None" : "";
      const maybeStar = spread ? "*" : "";
      name = sanitizeReservedWords(name);
      return `${maybeStar}${name}: ${pyType}${maybeDefault}`;
    },
  );
  formattedParams.unshift("self");
  if (sig.params.at(-1)?.spread) {
    formattedParams.splice(-1, 0, "/");
  } else {
    formattedParams.push("/");
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
  return `${name}: ${type}` + PROPERTY_TYPE_IGNORES;
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

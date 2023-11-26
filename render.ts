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
  return `class ${name}${supersList}:\n${body}`;
}

export function renderSignatureGroup(sigGroup: PySigGroup): string[] {
  const extraDecorators: string[] = [];
  const uniqueSigs = uniqBy(sigGroup.sigs, (sig) => JSON.stringify(sig));
  if (uniqueSigs.length > 1) {
    extraDecorators.push("overload");
  }

  return uniqueSigs.map((sig) =>
    renderSignature(sigGroup.name, sig, extraDecorators),
  );
}

function isIllegal(name) {
  return /["[$]/.test(name) || /^[0-9]/.test(name);
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

function sanitizeReservedWords(name) {
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
      const maybeDefault = optional ? "=None" : "";
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
  return `${decs}def ${name}(${joinedParams}) -> ${sig.returns}: ...`;
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
  return `${name}: ${type}`;
}

export function renderInnerSignature(sig: PySig): string {
  const paramTypes = sig.params.map(({ pyType }) => pyType);
  return `Callable[[${paramTypes.join(", ")}], ${sig.returns}]`;
}

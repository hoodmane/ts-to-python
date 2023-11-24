export type PyParam = { name: string; pyType: string; optional: boolean };
export type PySig = { params: PyParam[]; returns: string, decorators?: string[]};
export type PySigGroup = {name: string, sigs: PySig[]};

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
  body = indent(body, " ".repeat(4));
  let supersList = "";
  if (supers.length > 0) {
    supersList = `(${supers.join(", ")})`;
  }
  return `class ${name}${supersList}:\n${body}`;
}

export function renderSignatureGroup(
  sigGroup: PySigGroup
): string[] {
  const extraDecorators: string[] = [];
  if (sigGroup.sigs.length > 1) {
    extraDecorators.push("overload");
  }
  return sigGroup.sigs.map((sig) => renderSignature(sigGroup.name, sig, extraDecorators));
}

export function renderSignature(
  name: string,
  sig: PySig,
  extraDecorators: string[] = [],
): string {
  const formattedParams = sig.params.map(({ name, pyType, optional }) => {
    const maybeDefault = optional ? "=None" : "";
    return `${name}: ${pyType}${maybeDefault}`;
  });
  formattedParams.unshift("self");
  formattedParams.push("/");
  const joinedParams = formattedParams.join(", ");
  const decs = (sig.decorators || []).concat(extraDecorators).map((x) => "@" + x + "\n").join("");
  return `${decs}def ${name}(${joinedParams}) -> ${sig.returns}: ...`;
}

export function renderProperty(name: string, type: string): string {
  return renderSignature(name, { params: [], returns: type }, [
    "property",
  ]);
}

export function renderInnerSignature(sig: PySig): string {
  const paramTypes = sig.params.map(({ pyType }) => pyType);
  return `Callable[[${paramTypes.join(", ")}], ${sig.returns}]`;
}

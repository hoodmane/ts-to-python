export type PySig = { params: PyParam[]; returns: string };
export type PyParam = { name: string; pyType: string; optional: boolean };

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

export function renderTopLevelSignature(
  name: string,
  sig: PySig,
  decorators: string[] = [],
): string {
  const formattedParams = sig.params.map(({ name, pyType, optional }) => {
    const maybeDefault = optional ? "=None" : "";
    return `${name}: ${pyType}${maybeDefault}`;
  });
  formattedParams.unshift("self");
  formattedParams.push("/");
  const joinedParams = formattedParams.join(", ");
  const decs = decorators.map((x) => "@" + x + "\n").join("");
  return `${decs}def ${name}(${joinedParams}) -> ${sig.returns}: ...`;
}

export function renderProperty(name: string, type: string): string {
  return renderTopLevelSignature(name, { params: [], returns: type }, [
    "property",
  ]);
}

export function renderInnerSignature(sig: PySig): string {
  const paramTypes = sig.params.map(({ pyType }) => pyType);
  return `Callable[[${paramTypes.join(", ")}], ${sig.returns}]`;
}

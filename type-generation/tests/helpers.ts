import { SyntaxKind } from "ts-morph";
import { Converter } from "../extract";

let n = 0;
export function getTypeNode(converter: Converter, type) {
  n++;
  const fname = `/getTypeNode_$${n}.ts`;
  converter.project.createSourceFile(fname, `type A = ${type};`);
  const file = converter.project.getSourceFileOrThrow(fname);
  const alias = file.getFirstDescendantByKind(SyntaxKind.TypeAliasDeclaration);
  return alias.getTypeNode();
}

export function removeTypeIgnores(a: string) {
  return a.replaceAll(/\s*#.*$/gm, "");
}

export function dedent(s) {
  const lines = s.split("\n");
  let numSpaces = Infinity;
  for (const line of lines) {
    if (/^\s*$/.test(line)) {
      continue;
    }
    numSpaces = Math.min(numSpaces, /^\s*/.exec(line)[0].length);
  }
  return lines
    .map((line) => {
      if (/^\s*$/.test(line)) {
        return "";
      }
      return line.slice(numSpaces);
    })
    .join("\n");
}
import { Project, SyntaxKind } from "ts-morph";
import { Converter } from "../extract";
import { Converter as AstConverter } from "../astToIR";
import { TypeNode } from "ts-morph";

export function typeToIR(t: TypeNode) {
  return new AstConverter().typeToIR(t);
}

export function makeProject() {
  return new Project({
    tsConfigFilePath: "../type-generation-input-project/tsconfig.json",
    libFolderPath:
      "../type-generation-input-project/node_modules/typescript/lib",
  });
}

let n = 0;
export function getTypeNode(converter: Converter, type) {
  n++;
  const fname = `/getTypeNode_$${n}.ts`;
    const project = makeProject();
    project.createSourceFile(fname, `type A = ${type};`);
  const file = project.getSourceFileOrThrow(fname);
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

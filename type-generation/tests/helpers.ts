import { Project, SyntaxKind } from "ts-morph";
import { Converter as AstConverter } from "../src/astToIR";
import { TypeNode } from "ts-morph";

export function typeToIR(t: TypeNode) {
  const c = new AstConverter();
  return c.typeToIR(t);
}

export function makeProject(): Project {
  return new Project({
    tsConfigFilePath: "../input-projects/browser-dom/tsconfig.json",
    libFolderPath: "../input-projects/browser-dom/node_modules/typescript/lib",
  });
}

let n = 0;
export function getTypeNode(type: string): TypeNode {
  n++;
  const fname = `/getTypeNode_$${n}.ts`;
  const project = makeProject();
  project.createSourceFile(fname, `type A = ${type};`);
  const file = project.getSourceFileOrThrow(fname);
  const alias = file.getFirstDescendantByKind(SyntaxKind.TypeAliasDeclaration)!;
  return alias.getTypeNode()!;
}

export function removeTypeIgnores(a: string): string {
  return a.replaceAll(/\s*#.*$/gm, "");
}

export function dedent(s: string): string {
  const lines = s.split("\n");
  let numSpaces = Infinity;
  for (const line of lines) {
    if (/^\s*$/.test(line)) {
      continue;
    }
    numSpaces = Math.min(numSpaces, /^\s*/.exec(line)![0].length);
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

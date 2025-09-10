import { Project, SourceFile } from "ts-morph";
import { emitFiles } from "./extract.ts";
import { existsSync, mkdirSync, writeFileSync } from "fs";

Error.stackTraceLimit = Infinity;

function main() {
  let files: SourceFile[];
  const project = new Project({
    tsConfigFilePath: "../type-generation-input-project/tsconfig.json",
    libFolderPath:
      "../type-generation-input-project/node_modules/typescript/lib",
  });
  const sourceFile = "../type-generation-input-project/a.ts";
  project.addSourceFilesAtPaths(sourceFile);
  files = project.resolveSourceFileDependencies();
  files.push(project.getSourceFile(sourceFile))
  const result = emitFiles(files)
    .map((x) => x + "\n\n")
    .join("");
  const outDir = "../generated/js/";
  if (!existsSync(outDir)) {
    mkdirSync(outDir, { recursive: true });
  }
  writeFileSync(outDir + "__init__.pyi", result);
}
main();

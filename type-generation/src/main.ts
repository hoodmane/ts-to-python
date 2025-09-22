import { Project } from "ts-morph";
import { emitFiles } from "./extract.ts";
import { writeFileSync, readFileSync } from "fs";
import { resolve } from "path";

Error.stackTraceLimit = Infinity;

function main() {
  const projPath = process.argv.at(-2)!;
  const outFile = process.argv.at(-1)!;
  const tsConfigFilePath = resolve(projPath, "tsconfig.json");
  const project = new Project({
    tsConfigFilePath,
    libFolderPath: resolve(projPath, "node_modules/typescript/lib"),
  });
  const tsconfig = JSON.parse(
    readFileSync(tsConfigFilePath, { encoding: "utf8" }),
  ) as { include: string[] };
  const globs = tsconfig["include"].map((x) => resolve(projPath, x));
  const files = project.addSourceFilesAtPaths(globs);
  files.push(...project.resolveSourceFileDependencies());
  const result = emitFiles(files)
    .map((x) => x + "\n\n")
    .join("");
  writeFileSync(outFile, result);
}
main();

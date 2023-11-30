import { SourceFile } from "ts-morph";
import { Converter } from "./extract.ts";
import { existsSync, mkdirSync, writeFileSync } from "fs";

function main() {
    const converter = new Converter();
    let files: SourceFile[];
    converter.project.addSourceFilesAtPaths("../../input_example/a.ts");
    files = converter.project.resolveSourceFileDependencies();
    const result = converter.emit(files).map(x => x + "\n\n").join("");
    const outDir = "../generated/js/";
    if (!existsSync(outDir)) {
        mkdirSync(outDir, {recursive: true});
    }
    writeFileSync(outDir + "__init__.pyi", result);
}
main();


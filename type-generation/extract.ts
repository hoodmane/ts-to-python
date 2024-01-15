import { SourceFile } from "ts-morph";
import {
  PRELUDE,
  adjustFunction,
  adjustInterfaceIR,
  getExtraBases,
} from "./adjustments.ts";

import { InterfaceIR, convertFiles, ConversionResult } from "./astToIR.ts";
import { topLevelIRToString } from "./irToString.ts";

function topologicalSortClasses(
  nameToCls: Map<string, InterfaceIR>,
): InterfaceIR[] {
  type AnotatedClass = InterfaceIR & { sorted?: boolean; visited?: boolean };
  const result: InterfaceIR[] = [];
  function visit(cls: AnotatedClass) {
    if (cls.sorted) {
      return;
    }
    if (cls.visited) {
      throw new Error("Cycle");
    }
    cls.visited = true;
    for (const { name } of cls.bases) {
      const superClass = nameToCls.get(name);
      if (!superClass) {
        throw new Error(`Unknown super: ${cls.name} < ${name}`);
      }
      visit(superClass);
    }
    cls.sorted = true;
    result.push(cls);
  }
  for (const cls of nameToCls.values()) {
    visit(cls);
  }
  return result;
}

function fixupClassBases(unsortedClasses: InterfaceIR[]): void {
  const nameToCls = new Map(unsortedClasses.map((cls) => [cls.name, cls]));
  if (nameToCls.size < unsortedClasses.length) {
    throw new Error("Duplicate");
  }
  const classes = topologicalSortClasses(nameToCls);
  const classNameToIndex = new Map(classes.map((cls, idx) => [cls.name, idx]));
  for (const cls of classes) {
    cls.extraBases = getExtraBases(cls.name);
    if (cls.extraBases.length > 0) {
      cls.concrete = true;
    } else {
      for (const { name: sName } of cls.bases) {
        const s = nameToCls.get(sName);
        if (s.concrete) {
          cls.concrete = true;
          break;
        }
      }
    }
    if (cls.name.endsWith("_iface") && !cls.concrete) {
      cls.extraBases.push("Protocol");
    }
    if (!cls.name.endsWith("_iface")) {
      cls.extraBases.push("_JsObject");
    }
    cls.bases.sort(({ name: a }, { name: b }) => {
      return classNameToIndex.get(b) - classNameToIndex.get(a);
    });
  }
}

export function emitFiles(files: SourceFile[]): string[] {
  const result = convertFiles(files);
  return emitIR(result);
}

export function emitIR({ topLevels, typeParams }: ConversionResult): string[] {
  const unsortedClasses = topLevels.filter(
    (x): x is InterfaceIR => x.kind === "interface",
  );
  fixupClassBases(unsortedClasses);
  for (let cls of unsortedClasses) {
    adjustInterfaceIR(cls);
  }
  for (let obj of topLevels) {
    if (obj.kind === "callable") {
      adjustFunction(obj);
    }
    if (obj.kind === "interface") {
      obj.methods.forEach(adjustFunction);
    }
  }
  const typevarDecls = Array.from(
    typeParams,
    (x) => `${x} = TypeVar("${x}")`,
  ).join("\n");
  const rendered = topLevels.map((e) => topLevelIRToString(e));
  return [PRELUDE, typevarDecls, ...rendered];
}

import { SourceFile } from "ts-morph";
import {
  PRELUDE,
  adjustFunction,
  adjustInterfaceIR,
  getExtraBases,
  handleBuiltinBases,
} from "./adjustments.ts";

import {
  InterfaceIR,
  convertFiles,
  ConversionResult,
  TopLevels,
} from "./astToIR.ts";
import {
  callableIRToString,
  declarationIRToString,
  interfaceIRToString,
  typeAliasIRToString,
} from "./irToString.ts";

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

function fixupClassBases(nameToCls: Map<string, InterfaceIR>): void {
  handleBuiltinBases(nameToCls);
  const classes = topologicalSortClasses(nameToCls);
  const classNameToIndex = new Map(classes.map((cls, idx) => [cls.name, idx]));
  for (const cls of classes) {
    cls.extraBases ??= [];
    cls.extraBases.push(...getExtraBases(cls.name));
    if (cls.extraBases.length > 0) {
      cls.concrete = true;
    } else {
      for (const { name: sName } of cls.bases) {
        const s = nameToCls.get(sName);
        if (s?.concrete) {
          cls.concrete = true;
          break;
        }
        // If the base is a jsobject set the subclass to be a jsobject. This
        // probably shouldn't be necessary and indicates an issue somewhere
        // else.
        if (s?.jsobject) {
          cls.jsobject = true;
        }
      }
    }
    if (!cls.jsobject && !cls.concrete) {
      cls.extraBases.push("Protocol");
    }
    if (cls.jsobject) {
      cls.extraBases.push("_JsObject");
    }
    cls.bases.sort(({ name: a }, { name: b }) => {
      return classNameToIndex.get(b)! - classNameToIndex.get(a)!;
    });
  }
}

export function emitFiles(files: SourceFile[]): string[] {
  const result = convertFiles(files);
  return emitIR(result);
}

function adjustIR(topLevels: TopLevels): void {
  const classes = topLevels.ifaces;
  const nameToCls = new Map(classes.map((cls) => [cls.name, cls]));
  if (nameToCls.size < classes.length) {
    const once = new Set();
    const twice = new Set();
    for (const { name } of classes) {
      if (once.has(name)) {
        twice.add(name);
      } else {
        once.add(name);
      }
    }
    throw new Error(`Duplicate names: ${Array.from(twice).join(", ")}`);
  }
  fixupClassBases(nameToCls);
  classes.forEach(adjustInterfaceIR);
  topLevels.callables.forEach(adjustFunction);
  for (const iface of topLevels.ifaces) {
    iface.methods.forEach(adjustFunction);
  }
}

export function emitIR({ topLevels }: ConversionResult): string[] {
  adjustIR(topLevels);
  const typeAliasStrings = topLevels.typeAliases.map(typeAliasIRToString);
  const declarationStrings = topLevels.decls.map(declarationIRToString);
  const callableStrings = topLevels.callables.flatMap((tl) =>
    callableIRToString(tl, false),
  );
  const interfaceStrings = topLevels.ifaces.flatMap(interfaceIRToString);
  return [
    PRELUDE,
    ...typeAliasStrings,
    ...declarationStrings,
    ...callableStrings,
    ...interfaceStrings,
  ];
}

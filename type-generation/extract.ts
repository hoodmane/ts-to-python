import { SourceFile } from "ts-morph";
import {
  PRELUDE,
  adjustFunction,
  adjustInterfaceIR,
  getExtraBases,
} from "./adjustments.ts";

import {
  InterfaceIR,
  convertFiles,
  ConversionResult,
  TopLevels,
  TypeParamIR,
} from "./astToIR.ts";
import {
  callableIRToString,
  declarationIRToString,
  interfaceIRToString,
  typeAliasIRToString,
  typeParamIRToString,
} from "./irToString.ts";
import { topLevelIRToString, uniqBy } from "./irToString.ts";
import { Variance } from "./types.ts";

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

function adjustIR(topLevels: TopLevels): void {
  const classes = topLevels.ifaces;
  const nameToCls = new Map(classes.map((cls) => [cls.name, cls]));
  if (nameToCls.size < classes.length) {
    throw new Error("Duplicate");
  }
  fixupClassBases(nameToCls);
  classes.forEach(adjustInterfaceIR);
  topLevels.callables.forEach(adjustFunction);
  for (const iface of topLevels.ifaces) {
    iface.methods.forEach(adjustFunction);
  }
}

function getTypeVarDecls(
  classes: InterfaceIR[],
  funcParams: Set<string>,
): string {
  let typeVars: TypeParamIR[] = classes
    .flatMap(({ typeParams }) => typeParams)
    .map((param) => ({
      name: typeParamIRToString(param),
      variance: param.variance,
    }));
  typeVars.push(...Array.from(funcParams, (name) => ({ name })));
  typeVars = uniqBy(typeVars, ({ name }) => name);
  const typeVarDecls = typeVars.map(({ name, variance }) => {
    let varStr = "";
    if (variance === Variance.covar) {
      varStr = ", covariant=True";
    }
    if (variance === Variance.contra) {
      varStr = ", contravariant=True";
    }
    return `${name} = TypeVar("${name}"${varStr})`;
  });
  return typeVarDecls.join("\n");
}

export function emitIR({ topLevels, typeParams }: ConversionResult): string[] {
  adjustIR(topLevels);
  const typevarStrings = Array.from(
    typeParams,
    (x) => `${x} = TypeVar("${x}")`,
  ).join("\n");
  const typeAliasStrings = topLevels.typeAliases.map(typeAliasIRToString);
  const declarationStrings = topLevels.decls.map(declarationIRToString);
  const callableStrings = topLevels.callables.flatMap((tl) =>
    callableIRToString(tl, { isMethod: false }),
  );
  const interfaceStrings = topLevels.ifaces.flatMap(interfaceIRToString);
  return [
    PRELUDE,
    typevarStrings,
    ...typeAliasStrings,
    ...declarationStrings,
    ...callableStrings,
    ...interfaceStrings,
  ];
}

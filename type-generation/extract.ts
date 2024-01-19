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
  TopLevelIR,
  TypeParamIR,
  ConversionResult,
} from "./astToIR.ts";
import {
  topLevelIRToString,
  typeParamIRToString,
  uniqBy,
} from "./irToString.ts";
import { Variance } from "./types.ts";
import { calculateInterfaceTypeParamVariances } from "./varianceConstraints.ts";

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

export function emitFiles(files: SourceFile[], quiet?: boolean): string[] {
  const result = convertFiles(files);
  return emitIR(result);
}

function adjustIR(topLevels: TopLevelIR[]) {
  const classes = topLevels.filter(
    (x): x is InterfaceIR => x.kind === "interface",
  );
  const nameToCls = new Map(classes.map((cls) => [cls.name, cls]));
  if (nameToCls.size < classes.length) {
    throw new Error("Duplicate");
  }
  fixupClassBases(nameToCls);
  classes.forEach(adjustInterfaceIR);
  calculateInterfaceTypeParamVariances(nameToCls);

  for (let obj of topLevels) {
    if (obj.kind === "callable") {
      adjustFunction(obj);
    }
    if (obj.kind === "interface") {
      obj.methods.forEach(adjustFunction);
    }
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
  const classes = topLevels.filter(
    (x): x is InterfaceIR => x.kind === "interface",
  );
  const typevarDecls = getTypeVarDecls(classes, typeParams);
  const rendered = topLevels.map((e) => topLevelIRToString(e));
  return [PRELUDE, typevarDecls, ...rendered];
}

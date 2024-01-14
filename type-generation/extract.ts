import {
  ClassDeclaration,
  FunctionDeclaration,
  Identifier,
  InterfaceDeclaration,
  Node,
  Project,
  SourceFile,
  TypeElementTypes,
  VariableDeclaration,
} from "ts-morph";
import {
  PyParam,
  PySig,
  renderInnerSignature,
  renderProperty,
  renderPyClass,
  renderSignature,
  renderSignatureGroup,
  renderSimpleDeclaration,
  sanitizeReservedWords,
} from "./render.ts";
import { PyClass } from "./types.ts";
import {
  IMPORTS,
  getExtraBases,
  typeReferenceSubsitutions,
} from "./adjustments.ts";

import { groupBy, popElt } from "./groupBy.ts";
import {
  Needed,
  PyOther,
  PyTopLevel,
  Variance,
  reverseVariance,
} from "./types.ts";
import { assertUnreachable, classifyIdentifier } from "./astUtils.ts";
import {
  BaseIR,
  InterfaceIR,
  ParamIR,
  PropertyIR,
  CallableIR,
  SigIR,
  TopLevelIR,
  TypeIR,
  Converter as AstConverter,
} from "./astToIR.ts";

function pyClass(name: string, supers: string[], body: string): PyClass {
  const superStems = supers
    .map((sup) => sup.split("[")[0])
    .filter((x) => x !== "Generic");
  return {
    kind: "class",
    name,
    superStems,
    supers,
    body,
  };
}

function pyOther(text: string): PyOther {
  return {
    kind: "other",
    text,
  };
}

function topologicalSortClasses(nameToCls: Map<string, PyClass>): PyClass[] {
  type AnotatedClass = PyClass & { sorted?: boolean; visited?: boolean };
  const result: PyClass[] = [];
  function visit(cls: AnotatedClass) {
    if (cls.sorted) {
      return;
    }
    if (cls.visited) {
      throw new Error("Cycle");
    }
    cls.visited = true;
    for (const name of cls.superStems) {
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

export class Converter {
  astConverter: AstConverter;
  constructor() {
    this.astConverter = new AstConverter();
  }

  emit(files: SourceFile[]): string[] {
    const varDecls = files.flatMap((file) => file.getVariableDeclarations());

    const irTopLevels: TopLevelIR[] = [];
    for (const varDecl of varDecls) {
      const name = sanitizeReservedWords(varDecl.getName());
      if (this.astConverter.convertedSet.has(name)) {
        continue;
      }
      this.astConverter.convertedSet.add(name);
      const result = this.astConverter.varDeclToIR(varDecl);
      if (result) {
        irTopLevels.push(result);
      }
    }
    const funcDecls = files.flatMap((file) => file.getFunctions());
    const funcDeclsByName = groupBy(funcDecls, (decl) => decl.getName());
    for (const [name, decls] of Object.entries(funcDeclsByName)) {
      irTopLevels.push(this.astConverter.funcDeclsToIR(name, decls));
    }
    let next: Needed | undefined;
    while ((next = popElt(this.astConverter.neededSet))) {
      if (next.type === "ident") {
        let res = this.astConverter.identToIRIfNeeded(next.ident);
        if (res) {
          irTopLevels.push(res);
        }
        continue;
      }
      if (next.type === "interface") {
        const ident = next.ident;
        const name = ident.getText() + "_iface";
        if (this.astConverter.convertedSet.has(name)) {
          continue;
        }
        this.astConverter.convertedSet.add(name);

        const defs = ident
          .getDefinitionNodes()
          .filter(Node.isInterfaceDeclaration);
        if (defs.length) {
          const baseNames = this.astConverter
            .declsToBases(defs)
            .filter((base) => base.name !== name);
          const typeParams = defs
            .flatMap((i) => i.getTypeParameters())
            .map((p) => p.getName());
          const res = this.astConverter.interfaceToIR(
            name,
            baseNames,
            defs.flatMap((def) => def.getMembers()),
            [],
            typeParams,
          );
          irTopLevels.push(res);
          continue;
        }
        // console.warn(ident.getDefinitionNodes().map(n => n.getText()).join("\n\n"))
        console.warn("No interface declaration for " + name);
      }
    }
    const topLevels = irTopLevels.map((e) => this.renderTopLevelIR(e));
    const typevarDecls = Array.from(
      this.astConverter.typeParams,
      (x) => `${x} = TypeVar("${x}")`,
    ).join("\n");
    const output = [IMPORTS, typevarDecls];
    const unsortedClasses = topLevels.filter(
      (x): x is PyClass => x.kind === "class",
    );
    const nameToCls = new Map(unsortedClasses.map((cls) => [cls.name, cls]));
    if (nameToCls.size < unsortedClasses.length) {
      throw new Error("Duplicate");
    }
    // We need to ensure that the supers are topologically sorted so that we respect the MRO.
    const classes = topologicalSortClasses(nameToCls);
    const classNameToIndex = new Map(
      classes.map((cls, idx) => [cls.name, idx]),
    );
    for (const cls of classes) {
      const extraBases = getExtraBases(cls.name);
      if (extraBases) {
        cls.supers.push(...extraBases);
        cls.concrete = true;
      } else {
        for (const sName of cls.superStems) {
          const s = nameToCls.get(sName);
          if (s.concrete) {
            cls.concrete = true;
            break;
          }
        }
      }
      if (cls.name.endsWith("_iface") && !cls.concrete) {
        cls.supers.push("Protocol");
      }
      if (!cls.name.endsWith("_iface")) {
        cls.supers.push("_JsObject");
      }
    }
    for (const topLevel of topLevels) {
      switch (topLevel.kind) {
        case "class":
          topLevel.supers.sort((a, b) => {
            a = a.split("[")[0];
            b = b.split("[")[0];
            return classNameToIndex.get(b) - classNameToIndex.get(a);
          });
          output.push(renderPyClass(topLevel));
          break;

        case "other":
          const { text } = topLevel;
          output.push(text);
          break;
      }
    }
    return output;
  }

  renderTopLevelIR(toplevel: TopLevelIR): PyTopLevel {
    if (toplevel.kind === "declaration") {
      const { name, type } = toplevel;
      const typeStr = this.renderTypeIR(type, false, Variance.covar);
      return pyOther(renderSimpleDeclaration(name, typeStr));
    }
    if (toplevel.kind === "typeAlias") {
      const { name, type } = toplevel;
      const typeStr = this.renderTypeIR(type, false, Variance.covar);
      return pyOther(`${name} = ${typeStr}`);
    }
    if (toplevel.kind === "interface") {
      return this.renderInterface(toplevel);
    }
    if (toplevel.kind === "callable") {
      return pyOther(this.renderSignatureGroup(toplevel, false).join("\n"));
    }
    assertUnreachable(toplevel);
  }

  renderIRSignatures(
    irSigs: readonly SigIR[],
    variance: Variance,
    topLevelName?: string,
  ): string {
    const pySigs = irSigs.map((sig) => this.renderSig(sig, variance));
    if (!topLevelName) {
      const converted = pySigs.map(renderInnerSignature);
      return converted.join(" | ");
    }
    const converted = pySigs.map((sig) => renderSignature(topLevelName, sig));
    if (converted.length === 1) {
      return converted[0];
    }
    return converted.map((x) => "@overload\n" + x).join("\n\n");
  }

  renderSig(
    sig: SigIR,
    variance: Variance,
    decorators: string[] = [],
    isStatic: boolean = false,
  ): PySig {
    const renderParam = ({ name, optional, type }: ParamIR): PyParam => {
      const pyType = this.renderTypeIR(type, optional, paramVariance);
      return { name, optional, pyType };
    };
    const {
      params: origParams,
      spreadParam: origSpreadParam,
      kwparams: origKwparams,
      returns: origReturns,
    } = sig;
    const paramVariance = reverseVariance(variance);
    const params = origParams.map(renderParam);
    const kwparams = origKwparams?.map(renderParam);
    const spreadParam = origSpreadParam
      ? renderParam(origSpreadParam)
      : undefined;
    const returns = this.renderTypeIR(origReturns, false, variance);
    if (isStatic) {
      decorators.push("classmethod");
    }
    return { params, spreadParam, kwparams, returns, decorators };
  }

  renderSignatureGroup(
    { name, signatures: sigs, isStatic }: CallableIR,
    isMethod: boolean,
  ): string[] {
    const pySigs = sigs.map((sig) =>
      this.renderSig(sig, Variance.covar, [], isStatic),
    );
    return renderSignatureGroup({ name, sigs: pySigs }, isMethod);
  }

  renderBase({ name, typeParams }: BaseIR): string {
    if (typeParams.length > 0) {
      const joined = typeParams
        .map((t) => this.renderTypeIR(t, false, Variance.covar))
        .join(", ");
      name += `[${joined}]`;
    }
    return name;
  }

  renderInterface({
    name,
    properties,
    methods,
    typeParams,
    bases,
  }: InterfaceIR): PyClass {
    const entries = ([] as string[]).concat(
      properties.map((prop) => this.renderProperty(prop)),
      methods.flatMap((gp) => this.renderSignatureGroup(gp, true)),
    );
    const newSupers = bases.map((b) => this.renderBase(b));
    if (typeParams.length > 0) {
      const joined = typeParams.join(", ");
      newSupers.push(`Generic[${joined}]`);
    }
    return pyClass(name, newSupers, entries.join("\n"));
  }

  renderProperty(property: PropertyIR): string {
    const { isOptional, name, type, isReadonly, isStatic } = property;
    const pyType = this.renderTypeIR(type, isOptional, Variance.covar, name);
    return renderProperty(name, pyType, isReadonly, isStatic);
  }

  renderTypeIR(
    ir: TypeIR,
    isOptional: boolean,
    variance: Variance,
    topLevelName?: string,
  ): string {
    if (isOptional) {
      topLevelName = undefined;
    }
    let result = this.renderTypeIRInner(ir, variance, topLevelName);
    if (!isOptional) {
      return result;
    }
    if (
      (ir.kind === "simple" && ir.text === "None") ||
      (ir.kind === "union" &&
        ir.types.some((x) => x.kind === "simple" && x.text === "None"))
    ) {
      return result;
    }
    result += " | None";
    return result;
  }

  renderTypeIRInner(
    ir: TypeIR,
    variance: Variance,
    topLevelName?: string,
  ): string {
    if (ir.kind === "simple") {
      return ir.text;
    }
    if (ir.kind === "union") {
      return ir.types
        .map((ty) => this.renderTypeIR(ty, false, variance))
        .join(" | ");
    }
    if (ir.kind === "paren") {
      const inner = this.renderTypeIR(ir.type, false, variance);
      return `(${inner})`;
    }
    if (ir.kind === "array") {
      const eltType = this.renderTypeIR(ir.type, false, variance);
      if (variance === Variance.contra) {
        return `PyMutableSequence[${eltType}]`;
      }
      return `JsArray[${eltType}]`;
    }
    if (ir.kind == "tuple") {
      let elts = ir.types
        .map((elt) => this.renderTypeIR(elt, false, variance))
        .join(", ");
      if (elts === "") {
        elts = "()";
      }
      return `tuple[${elts}]`;
    }
    if (ir.kind === "operator") {
      // Ignore type operators
      return this.renderTypeIR(ir.type, false, variance);
    }
    if (ir.kind === "callable") {
      return this.renderIRSignatures(ir.signatures, variance, topLevelName);
    }
    if (ir.kind === "other") {
      return "Any";
    }
    if (ir.kind === "parameterReference") {
      return ir.name;
    }
    if (ir.kind === "reference") {
      let { identName: name, typeArgs } = ir;
      const res = typeReferenceSubsitutions(this, name, typeArgs, variance);
      if (res) {
        return res;
      }
      const args = typeArgs.map((ty) =>
        this.renderTypeIR(ty, false, Variance.none),
      );
      let fmtArgs = "";
      if (args.length) {
        fmtArgs = `[${args.join(", ")}]`;
      }
      return `${name}${fmtArgs}`;
    }
    assertUnreachable(ir);
  }
}

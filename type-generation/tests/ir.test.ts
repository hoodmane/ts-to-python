import { Converter } from "../extract";
import { typeToIR } from "../astToIR";
import { getTypeNode } from "./helpers";

function typeToIRHelper(tsType: string, converter?: Converter) {
  if (!converter) {
    converter = new Converter();
  }
  const typeNode = getTypeNode(converter, tsType);
  const ir = typeToIR(typeNode);
  if (ir.kind === "reference") {
    // @ts-expect-error
    ir.ident = ir.identName;
    ir.identName = undefined;
    ir.identIndex = undefined;
  }
  // console.dir(ir, { depth: null });
  return ir;
}

describe("typeToIR", () => {
  describe("basic", () => {
    it("convert string", () => {
      const typeIR = typeToIRHelper("string");
      expect(typeIR).toEqual({ kind: "simple", text: "str" });
    });
    it("convert number", () => {
      const typeIR = typeToIRHelper("number");
      expect(typeIR).toEqual({
        kind: "union",
        types: [
          { kind: "simple", text: "int" },
          { kind: "simple", text: "float" },
        ],
      });
    });
    it("convert union", () => {
      const typeIR = typeToIRHelper("string | boolean");
      expect(typeIR).toEqual({
        kind: "union",
        types: [
          { kind: "simple", text: "str" },
          { kind: "simple", text: "bool" },
        ],
      });
    });
    it("convert array", () => {
      const typeIR = typeToIRHelper("string[]");
      expect(typeIR).toEqual({
        kind: "array",
        type: { kind: "simple", text: "str" },
      });
    });
    it("convert readonly array", () => {
      const typeIR = typeToIRHelper("readonly string[]");
      expect(typeIR).toEqual({
        kind: "operator",
        operatorName: "readonly",
        type: { kind: "array", type: { kind: "simple", text: "str" } },
      });
    });
  });
  describe("typeReferenceSubsitutions", () => {
    it("convert Function", () => {
      const typeIR = typeToIRHelper("Function");
      expect(typeIR).toEqual({
        kind: "reference",
        ident: "Function",
        typeArgs: [],
      });
    });
    it("default type param", () => {
      const typeIR = typeToIRHelper("ReadableStream");
      expect(typeIR).toEqual({
        kind: "reference",
        ident: "ReadableStream",
        typeArgs: [{ kind: "simple", text: "Any" }],
      });
    });
    it("convert Promise", () => {
      const typeIR = typeToIRHelper("Promise<string | symbol>");
      expect(typeIR).toEqual({
        kind: "reference",
        ident: "Promise",
        typeArgs: [
          {
            kind: "union",
            types: [
              { kind: "simple", text: "str" },
              { kind: "simple", text: "Symbol" },
            ],
          },
        ],
      });
    });
    it("convert Iterator", () => {
      const converter = new Converter();
      let typeIR;
      typeIR = typeToIRHelper("Iterator<boolean>", converter);
      expect(typeIR).toEqual({
        kind: "reference",
        ident: "Iterator",
        typeArgs: [
          { kind: "simple", text: "bool" },
          { kind: "simple", text: "Any" },
          { kind: "simple", text: "None" },
        ],
      });
      typeIR = typeToIRHelper("Iterator<boolean, string, symbol>", converter);
      expect(typeIR).toEqual({
        kind: "reference",
        ident: "Iterator",
        typeArgs: [
          { kind: "simple", text: "bool" },
          { kind: "simple", text: "str" },
          { kind: "simple", text: "Symbol" },
        ],
      });
    });
  });
  describe("callable types", () => {
    it("basic", () => {
      const typeIR = typeToIRHelper("() => void");
      expect(typeIR).toEqual({
        kind: "callable",
        signatures: [
          {
            params: [],
            spreadParam: undefined,
            returns: { kind: "simple", text: "None" },
          },
        ],
      });
    });
    it("optional args", () => {
      const typeIR = typeToIRHelper("(a?: string) => void");
      expect(typeIR).toEqual({
        kind: "callable",
        signatures: [
          {
            params: [
              {
                name: "a",
                type: { kind: "simple", text: "str" },
                optional: true,
              },
            ],
            spreadParam: undefined,
            returns: { kind: "simple", text: "None" },
          },
        ],
      });
    });
    it("optional or null", () => {
      const typeIR = typeToIRHelper("(a?: string | null) => void;");
      expect(typeIR).toEqual({
        kind: "callable",
        signatures: [
          {
            params: [
              {
                name: "a",
                type: {
                  kind: "union",
                  types: [
                    { kind: "simple", text: "str" },
                    { kind: "simple", text: "None" },
                  ],
                },
                optional: true,
              },
            ],
            spreadParam: undefined,
            returns: { kind: "simple", text: "None" },
          },
        ],
      });
    });
    it("type predicate", () => {
      const typeIR = typeToIRHelper("(a: any) => a is string;");
      expect(typeIR).toEqual({
        kind: "callable",
        signatures: [
          {
            params: [
              {
                name: "a",
                type: { kind: "simple", text: "Any" },
                optional: false,
              },
            ],
            spreadParam: undefined,
            returns: { kind: "simple", text: "bool" },
          },
        ],
      });
    });
    it("spread arg", () => {
      const typeIR = typeToIRHelper("(...a: string[][]) => void;");
      expect(typeIR).toEqual({
        kind: "callable",
        signatures: [
          {
            params: [],
            spreadParam: {
              name: "a",
              type: { kind: "array", type: { kind: "simple", text: "str" } },
              optional: false,
            },
            returns: { kind: "simple", text: "None" },
          },
        ],
      });
    });
  });
});

import { getTypeNode, typeToIR } from "./helpers";

function typeToIRHelper(tsType: string) {
  const typeNode = getTypeNode(tsType);
  const ir = typeToIR(typeNode);
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
        kind: "number",
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
        name: "Function",
        typeArgs: [],
      });
    });
    it("default type param", () => {
      const typeIR = typeToIRHelper("ReadableStream");
      expect(typeIR).toEqual({
        kind: "reference",
        name: "ReadableStream",
        typeArgs: [{ kind: "simple", text: "Any" }],
      });
    });
    it("convert Promise", () => {
      const typeIR = typeToIRHelper("Promise<string | symbol>");
      expect(typeIR).toEqual({
        kind: "reference",
        name: "Promise",
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
      let typeIR;
      typeIR = typeToIRHelper("Iterator<boolean>");
      expect(typeIR).toEqual({
        kind: "reference",
        name: "Iterator_iface",
        typeArgs: [
          { kind: "simple", text: "bool" },
          { kind: "simple", text: "Any" },
          { kind: "simple", text: "None" },
        ],
      });
      typeIR = typeToIRHelper("Iterator<boolean, string, symbol>");
      expect(typeIR).toEqual({
        kind: "reference",
        name: "Iterator_iface",
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
                isOptional: true,
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
                isOptional: true,
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
                isOptional: false,
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
              isOptional: false,
            },
            returns: { kind: "simple", text: "None" },
          },
        ],
      });
    });
  });
});

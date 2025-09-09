import { describe, it } from "node:test";
import assert from "node:assert";
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
      assert.deepStrictEqual(typeIR, { kind: "simple", text: "str" });
    });
    it("convert number", () => {
      const typeIR = typeToIRHelper("number");
      assert.deepStrictEqual(typeIR, {
        kind: "number",
      });
    });
    it("convert union", () => {
      const typeIR = typeToIRHelper("string | boolean");
      assert.deepStrictEqual(typeIR, {
        kind: "union",
        types: [
          { kind: "simple", text: "str" },
          { kind: "simple", text: "bool" },
        ],
      });
    });
    it("convert array", () => {
      const typeIR = typeToIRHelper("string[]");
      assert.deepStrictEqual(typeIR, {
        kind: "array",
        type: { kind: "simple", text: "str" },
      });
    });
    it("convert readonly array", () => {
      const typeIR = typeToIRHelper("readonly string[]");
      assert.deepStrictEqual(typeIR, {
        kind: "operator",
        operatorName: "readonly",
        type: { kind: "array", type: { kind: "simple", text: "str" } },
      });
    });
  });
  describe("typeReferenceSubsitutions", () => {
    it("convert Function", () => {
      const typeIR = typeToIRHelper("Function");
      assert.deepStrictEqual(typeIR, {
        kind: "reference",
        name: "Function",
        typeArgs: [],
      });
    });
    it("default type param", () => {
      const typeIR = typeToIRHelper("ReadableStream");
      assert.deepStrictEqual(typeIR, {
        kind: "reference",
        name: "ReadableStream",
        typeArgs: [{ kind: "simple", text: "Any" }],
      });
    });
    it("convert Promise", () => {
      const typeIR = typeToIRHelper("Promise<string | symbol>");
      assert.deepStrictEqual(typeIR, {
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
      assert.deepStrictEqual(typeIR, {
        kind: "reference",
        name: "Iterator_iface",
        typeArgs: [
          { kind: "simple", text: "bool" },
          { kind: "simple", text: "Any" },
          { kind: "simple", text: "None" },
        ],
      });
      typeIR = typeToIRHelper("Iterator<boolean, string, symbol>");
      assert.deepStrictEqual(typeIR, {
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
      assert.deepStrictEqual(typeIR, {
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
      assert.deepStrictEqual(typeIR, {
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
      assert.deepStrictEqual(typeIR, {
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
      assert.deepStrictEqual(typeIR, {
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
      assert.deepStrictEqual(typeIR, {
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

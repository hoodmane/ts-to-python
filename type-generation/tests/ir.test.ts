import { describe, it } from "node:test";
import assert from "node:assert";
import { getTypeNode, typeToIR, makeProject } from "./helpers";
import { typeAliasIRToString } from "../src/irToString.ts";
import { Converter } from "../src/astToIR.ts";
import { SyntaxKind } from "ts-morph";

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
  describe("function declarations", () => {
    it("simple function", () => {
      const project = makeProject();
      project.createSourceFile(
        "/test.ts",
        `declare function f(x: string): void;`,
      );
      const file = project.getSourceFileOrThrow("/test.ts");
      const funcDecl = file.getFirstDescendantByKind(
        SyntaxKind.FunctionDeclaration,
      )!;
      const converter = new Converter();
      const ir = converter.funcDeclsToIR("f", [funcDecl]);

      assert.deepStrictEqual(ir, {
        kind: "callable",
        name: "f",
        signatures: [
          {
            params: [
              {
                name: "x",
                type: { kind: "simple", text: "str" },
                isOptional: false,
              },
            ],
            spreadParam: undefined,
            returns: { kind: "simple", text: "None" },
          },
        ],
        isStatic: false,
      });
    });

    it("generic function", () => {
      const project = makeProject();
      project.createSourceFile("/test.ts", `declare function f<T>(x: T): T;`);
      const file = project.getSourceFileOrThrow("/test.ts");
      const funcDecl = file.getFirstDescendantByKind(
        SyntaxKind.FunctionDeclaration,
      )!;
      const converter = new Converter();
      const ir = converter.funcDeclsToIR("f", [funcDecl]);

      assert.deepStrictEqual(ir, {
        kind: "callable",
        name: "f",
        signatures: [
          {
            params: [
              {
                name: "x",
                type: { kind: "parameterReference", name: "T" },
                isOptional: false,
              },
            ],
            spreadParam: undefined,
            returns: { kind: "parameterReference", name: "T" },
            typeParams: ["T"],
          },
        ],
        isStatic: false,
      });
    });
  });
  describe("interface method type params", () => {
    it("simple interface method", () => {
      const project = makeProject();
      project.createSourceFile(
        "/test.ts",
        `
        interface Test {
          method(x: string): void;
        }
      `,
      );
      const file = project.getSourceFileOrThrow("/test.ts");
      const iface = file.getFirstDescendantByKind(
        SyntaxKind.InterfaceDeclaration,
      )!;
      const method = iface.getFirstDescendantByKind(
        SyntaxKind.MethodSignature,
      )!;
      const signature = method.getSignature();

      const converter = new Converter();
      const ir = converter.callableToIR("method", [signature], false);

      assert.deepStrictEqual(ir, {
        kind: "callable",
        name: "method",
        signatures: [
          {
            params: [
              {
                name: "x",
                type: { kind: "simple", text: "str" },
                isOptional: false,
              },
            ],
            spreadParam: undefined,
            returns: { kind: "simple", text: "None" },
          },
        ],
        isStatic: false,
      });
    });

    it("generic interface method", () => {
      const project = makeProject();
      project.createSourceFile(
        "/test.ts",
        `
        interface Test {
          method<T>(x: T): T;
        }
      `,
      );
      const file = project.getSourceFileOrThrow("/test.ts");
      const iface = file.getFirstDescendantByKind(
        SyntaxKind.InterfaceDeclaration,
      )!;
      const method = iface.getFirstDescendantByKind(
        SyntaxKind.MethodSignature,
      )!;
      const signature = method.getSignature();

      const converter = new Converter();
      const ir = converter.callableToIR("method", [signature], false);

      assert.deepStrictEqual(ir, {
        kind: "callable",
        name: "method",
        signatures: [
          {
            params: [
              {
                name: "x",
                type: { kind: "parameterReference", name: "T" },
                isOptional: false,
              },
            ],
            spreadParam: undefined,
            returns: { kind: "parameterReference", name: "T" },
            typeParams: ["T"],
          },
        ],
        isStatic: false,
      });
    });

    it("constructor method", () => {
      const project = makeProject();
      project.createSourceFile(
        "/test.ts",
        `
        interface TestConstructor {
          new<T>(): Test<T>;
        }
      `,
      );
      const file = project.getSourceFileOrThrow("/test.ts");
      const iface = file.getFirstDescendantByKind(
        SyntaxKind.InterfaceDeclaration,
      )!;
      const constructors = iface
        .getConstructSignatures()
        .map((decl) => decl.getSignature());

      const converter = new Converter();
      const ir = converter.callableToIR("new", constructors, true);

      assert.deepStrictEqual(ir, {
        kind: "callable",
        name: "new",
        signatures: [
          {
            params: [],
            spreadParam: undefined,
            returns: {
              kind: "reference",
              name: "Test_iface",
              typeArgs: [{ kind: "parameterReference", name: "T" }],
            },
            typeParams: ["T"],
          },
        ],
        isStatic: true,
      });
    });
  });
  describe("type aliases", () => {
    it("simple type alias", () => {
      const project = makeProject();
      project.createSourceFile(
        "/test.ts",
        `
        type MyString = string;
        declare var x: MyString;
      `,
      );
      const file = project.getSourceFileOrThrow("/test.ts");
      const varDecl = file.getFirstDescendantByKind(
        SyntaxKind.VariableDeclaration,
      )!;
      const typeRef = varDecl.getTypeNode()!;
      const ident = typeRef.getFirstDescendantByKind(SyntaxKind.Identifier)!;

      const converter = new Converter();
      const ir = converter.identToIR(ident);

      assert.deepStrictEqual(ir, {
        kind: "typeAlias",
        name: "MyString",
        type: { kind: "simple", text: "str" },
        typeParams: [],
      });
    });

    it("generic type alias", () => {
      const project = makeProject();
      project.createSourceFile(
        "/test.ts",
        `
        type MyType<T> = T | string;
        declare var x: MyType<number>;
      `,
      );
      const file = project.getSourceFileOrThrow("/test.ts");
      const varDecl = file.getFirstDescendantByKind(
        SyntaxKind.VariableDeclaration,
      )!;
      const typeRef = varDecl.getTypeNode()!;
      const ident = typeRef.getFirstDescendantByKind(SyntaxKind.Identifier)!;

      const converter = new Converter();
      const ir = converter.identToIR(ident);

      assert.deepStrictEqual(ir, {
        kind: "typeAlias",
        name: "MyType",
        type: {
          kind: "union",
          types: [
            { kind: "parameterReference", name: "T" },
            { kind: "simple", text: "str" },
          ],
        },
        typeParams: ["T"],
      });
    });
  });
});

import { describe, it } from "node:test";
import assert from "node:assert";
import { getTypeNode, typeToIR, makeProject } from "./helpers";
import { Converter } from "../src/astToIR.ts";
import { SyntaxKind } from "ts-morph";

function typeToIRHelper(tsType: string) {
  const typeNode = getTypeNode(tsType);
  const ir = typeToIR(typeNode);
  // console.dir(ir, { depth: null });
  return ir;
}

function makeTestSourceFile(body: string) {
  const project = makeProject();
  project.createSourceFile("/test.ts", body);
  return project.getSourceFileOrThrow("/test.ts");
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
      const file = makeTestSourceFile(`declare function f(x: string): void;`);
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
      const file = makeTestSourceFile(`declare function f<T>(x: T): T;`);
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
      const file = makeTestSourceFile(`
        interface Test {
          method(x: string): void;
        }
      `);
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
      const file = makeTestSourceFile(`
        interface Test {
          method<T>(x: T): T;
        }
      `);
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
      const file = makeTestSourceFile(`
        interface TestConstructor {
          new<T>(): Test<T>;
        }
      `);
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
      const file = makeTestSourceFile(`
        type MyString = string;
        declare var x: MyString;
      `);
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
      const file = makeTestSourceFile(`
        type MyType<T> = T | string;
        declare var x: MyType<number>;
      `);
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
  describe("per-signature type parameters", () => {
    it("mixed constructor overloads with different type params", () => {
      const file = makeTestSourceFile(`
        interface XIface<T> {
          x: T;
        }
        interface XConstructor {
          new (x?: number): XIface<any>;
          new <T>(x: number): XIface<T>;
        }
      `);
      const interfaces = file.getDescendantsOfKind(
        SyntaxKind.InterfaceDeclaration,
      );
      const constructorInterface = interfaces.find(
        (decl) => decl.getName() === "XConstructor",
      )!;
      const constructors = constructorInterface
        .getConstructSignatures()
        .map((decl) => decl.getSignature());

      const converter = new Converter();
      const ir = converter.callableToIR("new", constructors, true);

      assert.deepStrictEqual(ir, {
        kind: "callable",
        name: "new",
        signatures: [
          {
            params: [
              {
                name: "x",
                type: { kind: "number" },
                isOptional: true,
              },
            ],
            spreadParam: undefined,
            returns: {
              kind: "reference",
              name: "XIface_iface",
              typeArgs: [{ kind: "simple", text: "Any" }],
            },
          },
          {
            params: [
              {
                name: "x",
                type: { kind: "number" },
                isOptional: false,
              },
            ],
            spreadParam: undefined,
            returns: {
              kind: "reference",
              name: "XIface_iface",
              typeArgs: [{ kind: "parameterReference", name: "T" }],
            },
            typeParams: ["T"],
          },
        ],
        isStatic: true,
      });
    });

    it("interface method with destructured parameters preserves type params", () => {
      const file = makeTestSourceFile(`
        interface Options<T> {
          value: T;
        }
        interface Test {
          method<T>(x: T, options?: Options<T>): T[];
        }
      `);
      const interfaces = file.getDescendantsOfKind(
        SyntaxKind.InterfaceDeclaration,
      );
      const testInterface = interfaces.find(
        (decl) => decl.getName() === "Test",
      )!;
      const method = testInterface.getFirstDescendantByKind(
        SyntaxKind.MethodSignature,
      )!;
      const signature = method.getSignature();

      const converter = new Converter();
      const ir = converter.callableToIR("method", [signature], false);

      // Should have 2 signatures: original and destructured
      assert.strictEqual(ir.signatures.length, 2);

      // Both signatures should have type parameters
      assert.deepStrictEqual(ir.signatures[0].typeParams, ["T"]);
      assert.deepStrictEqual(ir.signatures[1].typeParams, ["T"]);

      // Second signature should have destructured kwparams
      assert.strictEqual(ir.signatures[1].kwparams?.length, 1);
      assert.strictEqual(ir.signatures[1].kwparams?.[0].name, "value");
      assert.deepStrictEqual(ir.signatures[1].kwparams?.[0].type, {
        kind: "parameterReference",
        name: "T",
      });
    });

    it("function with multiple overloads each with different type params", () => {
      const file = makeTestSourceFile(`
        declare function process<T>(input: T): T;
        declare function process<U, V>(first: U, second: V): U | V;
        `);
      const funcDecls = file.getDescendantsOfKind(
        SyntaxKind.FunctionDeclaration,
      );

      const converter = new Converter();
      const ir = converter.funcDeclsToIR("process", funcDecls);

      assert.strictEqual(ir.signatures.length, 2);

      // First signature should have typeParams: ["T"]
      assert.deepStrictEqual(ir.signatures[0].typeParams, ["T"]);
      assert.deepStrictEqual(ir.signatures[0].returns, {
        kind: "parameterReference",
        name: "T",
      });

      // Second signature should have typeParams: ["U", "V"]
      assert.deepStrictEqual(ir.signatures[1].typeParams, ["U", "V"]);
      assert.deepStrictEqual(ir.signatures[1].returns, {
        kind: "union",
        types: [
          { kind: "parameterReference", name: "U" },
          { kind: "parameterReference", name: "V" },
        ],
      });
    });

    it("destructured type parameter resolution", () => {
      const file = makeTestSourceFile(`
        interface X<R> {
          r: R;
        }

        interface S<T> {
          t: T;
        }

        declare var X: {
          new<R = any>(source: R, strategy?: S<R>): X<R>;
        };
      `);
      const varDecl = file.getFirstDescendantByKind(
        SyntaxKind.VariableDeclaration,
      )!;
      const typeLiteral = varDecl
        .getTypeNode()!
        .asKind(SyntaxKind.TypeLiteral)!;
      const constructSignature = typeLiteral.getConstructSignatures()[0];
      const signature = constructSignature.getSignature();

      const converter = new Converter();
      const ir = converter.callableToIR("new", [signature], true);

      // Should have 2 signatures: original and destructured
      assert.strictEqual(ir.signatures.length, 2);

      // Both signatures should have type parameters
      assert.deepStrictEqual(ir.signatures[0].typeParams, ["R"]);
      assert.deepStrictEqual(ir.signatures[1].typeParams, ["R"]);

      // Second signature should have destructured kwparams with resolved type
      assert.strictEqual(ir.signatures[1].kwparams?.length, 1);
      assert.strictEqual(ir.signatures[1].kwparams?.[0].name, "t");

      // The destructured property should resolve T -> R
      assert.deepStrictEqual(ir.signatures[1].kwparams?.[0].type, {
        kind: "parameterReference",
        name: "R",
      });
    });

    it("destructured type parameter resolution with concrete type", () => {
      const file = makeTestSourceFile(`
        interface X<R> {
          r: R;
        }

        interface S<T> {
          t: T;
        }

        declare var X: {
          new<R = any>(source: R, strategy?: S<string>): X<R>;
        };
      `);
      const varDecl = file.getFirstDescendantByKind(
        SyntaxKind.VariableDeclaration,
      )!;
      const typeLiteral = varDecl
        .getTypeNode()!
        .asKind(SyntaxKind.TypeLiteral)!;
      const constructSignature = typeLiteral.getConstructSignatures()[0];
      const signature = constructSignature.getSignature();

      const converter = new Converter();
      const ir = converter.callableToIR("new", [signature], true);

      // Should have 2 signatures: original and destructured
      assert.strictEqual(ir.signatures.length, 2);

      // Both signatures should have type parameters
      assert.deepStrictEqual(ir.signatures[0].typeParams, ["R"]);
      assert.deepStrictEqual(ir.signatures[1].typeParams, ["R"]);

      // Second signature should have destructured kwparams with resolved type
      assert.strictEqual(ir.signatures[1].kwparams?.length, 1);
      assert.strictEqual(ir.signatures[1].kwparams?.[0].name, "t");

      // The destructured property should resolve T -> string
      assert.deepStrictEqual(ir.signatures[1].kwparams?.[0].type, {
        kind: "simple",
        text: "str",
      });
    });

    it("complex destructured interface with multiple type parameters", () => {
      const file = makeTestSourceFile(`
        interface Xiface<R> {
          r: R;
        }

        interface Size<T = any> {
          (chunk: T): number;
        }

        interface S<T> {
          t: T;
          s: Size<T>
        }

        declare var X: {
          new<R = any>(source: R, strategy?: S<R>): Xiface<R>;
        };
      `);
      const varDecl = file.getFirstDescendantByKind(
        SyntaxKind.VariableDeclaration,
      )!;
      const typeLiteral = varDecl
        .getTypeNode()!
        .asKind(SyntaxKind.TypeLiteral)!;
      const constructSignatures = typeLiteral.getConstructSignatures();
      const signatures = constructSignatures.map((cs) => cs.getSignature());

      const converter = new Converter();
      const ir = converter.callableToIR("new", signatures, true);

      // Should have 2 signatures: 1 original + 1 destructured
      assert.strictEqual(ir.signatures.length, 2);

      // All signatures should have type parameters
      assert.deepStrictEqual(ir.signatures[0].typeParams, ["R"]);
      assert.deepStrictEqual(ir.signatures[1].typeParams, ["R"]);

      // Second and fourth signatures should have destructured kwparams with resolved types
      assert.strictEqual(ir.signatures[1].kwparams?.length, 2);
      assert.strictEqual(ir.signatures[1].kwparams?.[0].name, "t");
      assert.strictEqual(ir.signatures[1].kwparams?.[1].name, "s");

      // The destructured properties should resolve T -> R
      assert.deepStrictEqual(ir.signatures[1].kwparams?.[0].type, {
        kind: "parameterReference",
        name: "R",
      });
      assert.deepStrictEqual(ir.signatures[1].kwparams?.[1].type, {
        kind: "reference",
        name: "Size_iface",
        typeArgs: [{ kind: "parameterReference", name: "R" }],
      });
    });
  });
  describe("class type parameter handling", () => {
    it("avoids duplicated class and method type params", () => {
      const file = makeTestSourceFile(`
        interface Test<T> {
          method<T>(x: T): T;
          method2(y: T): T;
        }
      `);
      const iface = file.getFirstDescendantByKind(
        SyntaxKind.InterfaceDeclaration,
      )!;

      const converter = new Converter();
      const ir = converter.interfaceToIR(
        "Test",
        [],
        iface.getMembers(),
        [],
        [],
        ["T"],
      );

      // Method with same-named type param should not duplicate it
      const method1 = ir.methods.find((m) => m.name === "method")!;
      assert.strictEqual(method1.signatures[0].typeParams, undefined);

      // Method using class type param should work correctly
      const method2 = ir.methods.find((m) => m.name === "method2")!;
      assert.deepStrictEqual(method2.signatures[0].returns, {
        kind: "parameterReference",
        name: "T",
      });
    });
    it("includes class type params in destructured type parameter resolution", () => {
      const file = makeTestSourceFile(`
        interface Strategy<S> {
          s: S;
        }

        interface Options<T> {
          s: Strategy<T>
        }

        interface Xyz<R> {
          r: R;
          method<U>(other: U, strategy?: Options<R>): R | U;
        }
      `);
      const interfaces = file.getDescendantsOfKind(
        SyntaxKind.InterfaceDeclaration,
      );
      const xyzInterface = interfaces.find((decl) => decl.getName() === "Xyz")!;
      const method = xyzInterface.getFirstDescendantByKind(
        SyntaxKind.MethodSignature,
      )!;
      const signature = method.getSignature();

      const converter = new Converter();
      // Set class type params to simulate being inside a class
      converter.classTypeParams.add("R");
      const ir = converter.callableToIR("method", [signature], false);

      // Should have 2 signatures: original and destructured
      assert.strictEqual(ir.signatures.length, 2);

      // Both signatures should have only method-level type parameters
      assert.deepStrictEqual(ir.signatures[0].typeParams, ["U"]);
      assert.deepStrictEqual(ir.signatures[1].typeParams, ["U"]);

      // Second signature should have destructured kwparams with class type param resolved
      assert.strictEqual(ir.signatures[1].kwparams?.length, 1);
      assert.strictEqual(ir.signatures[1].kwparams?.[0].name, "s");

      // The destructured property should resolve to Strategy<R> where R is class type param
      assert.deepStrictEqual(ir.signatures[1].kwparams?.[0].type, {
        kind: "reference",
        name: "Strategy_iface",
        typeArgs: [{ kind: "parameterReference", name: "R" }],
      });
    });
  });
  describe("interface inheritance", () => {
    it("extends Record creates interface with __getattr__", () => {
      const file = makeTestSourceFile(`
        interface I extends Record<string, number> {
          x: string;
        }
      `);
      const iface = file.getFirstDescendantByKind(
        SyntaxKind.InterfaceDeclaration,
      )!;

      const converter = new Converter();
      const bases = converter.getBasesOfDecls([iface]);
      const ir = converter.interfaceToIR(
        "I",
        bases,
        iface.getMembers(),
        [],
        [],
        [],
      );

      // Should have no bases since Record is filtered out
      assert.deepStrictEqual(ir.bases, []);

      // Should have the regular property
      assert.strictEqual(ir.properties.length, 1);
      assert.strictEqual(ir.properties[0].name, "x");
      assert.deepStrictEqual(ir.properties[0].type, {
        kind: "simple",
        text: "str",
      });

      // Should have the __getattr__ method for Record functionality
      const getattrMethod = ir.methods.find((m) => m.name === "__getattr__");
      assert.ok(getattrMethod, "Should have __getattr__ method");
      assert.strictEqual(getattrMethod.signatures.length, 1);
      assert.strictEqual(getattrMethod.signatures[0].params.length, 1);
      assert.strictEqual(getattrMethod.signatures[0].params[0].name, "key");
      assert.deepStrictEqual(getattrMethod.signatures[0].params[0].type, {
        kind: "simple",
        text: "str",
      });
      assert.deepStrictEqual(getattrMethod.signatures[0].returns, {
        kind: "number",
      });
    });
  });
});

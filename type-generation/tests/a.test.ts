import { describe, it } from "node:test";
import assert from "node:assert";
import {
  ClassDeclaration,
  FunctionDeclaration,
  Identifier,
  InterfaceDeclaration,
  Node,
  PropertySignature,
  SyntaxKind,
  TypeNode,
  VariableDeclaration,
} from "ts-morph";
import { emitFiles, emitIR } from "../src/extract.ts";
import {
  propertyIRToString,
  baseIRToString,
  callableIRToString,
  typeIRToString,
  declarationIRToString,
  interfaceIRToString,
  typeAliasIRToString,
} from "../src/irToString.ts";
import { Variance } from "../src/types.ts";
import {
  dedent,
  getTypeNode,
  makeProject,
  removeTypeIgnores,
  typeToIR,
} from "./helpers.ts";
import {
  Converter as AstConverter,
  ConversionResult,
  convertDecls,
} from "../src/astToIR";

function propertySignatureToIR(
  member: PropertySignature,
  isStatic: boolean = false,
) {
  return new AstConverter().propertySignatureToIR(member, isStatic);
}

function checkTypeToPython(
  tsType: string,
  pyType: string,
  variance: Variance = Variance.covar,
) {
  const typeNode = getTypeNode(tsType);
  const conversion = typeAstToString(typeNode, false, variance);
  assert.strictEqual(conversion, pyType);
}

function typeAstToString(
  typeNode: TypeNode,
  isOptional: boolean,
  variance: Variance,
  topLevelName?: string,
): string {
  const ir = typeToIR(typeNode);
  return typeIRToString(ir, { isOptional, variance, topLevelName });
}

function propertySignatureAstToString(
  member: PropertySignature,
  isStatic: boolean = false,
): string {
  return propertyIRToString(propertySignatureToIR(member, isStatic));
}

function convertBuiltinFunction(funcName: string): ConversionResult {
  const project = makeProject();
  project.createSourceFile("/a.ts", funcName);
  const x = project.getSourceFileOrThrow("/a.ts");
  const id = x.getStatements()[0].getChildren()[0] as Identifier;
  const funcDecl = id.getDefinitionNodes()[0] as FunctionDeclaration;
  return convertDecls([], [funcDecl], []);
}

function emitIRNoTypeIgnores(x: ConversionResult): string[] {
  return emitIR(x).map(removeTypeIgnores);
}

function convertBuiltinVariable(varName: string): string[] {
  const project = makeProject();
  project.createSourceFile("/a.ts", varName);
  const x = project.getSourceFileOrThrow("/a.ts");
  const id = x.getStatements()[0].getChildren()[0] as Identifier;
  console.log(id.getDefinitionNodes().map((x) => x.getKindName()));
  // process.exit(1);
  const varDecl = id
    .getDefinitionNodes()
    .filter(Node.isVariableDeclaration)[0]!;
  const ir = convertDecls([varDecl], [], []);
  return emitIR(ir).map(removeTypeIgnores);
}

describe("typeToPython", () => {
  it("convert string", () => {
    checkTypeToPython("string", "str");
  });
  it("convert number", () => {
    checkTypeToPython("number", "int | float");
  });
  it("convert union", () => {
    checkTypeToPython("string | boolean", "str | bool");
  });
  it("convert array", () => {
    checkTypeToPython("string[]", "JsArray[str]");
    checkTypeToPython("string[]", "PyMutableSequence[str]", Variance.contra);
  });
  describe("typeReferenceSubsitutions", () => {
    it("convert Function", () => {
      checkTypeToPython("Function", "Callable[..., Any]");
    });
    it("convert Exclude", () => {
      checkTypeToPython("Exclude<string | symbol, string>", "str | Symbol");
    });
    it("convert Readonly", () => {
      checkTypeToPython("Readonly<string | symbol>", "str | Symbol");
    });
    it("convert Promise", () => {
      checkTypeToPython("Promise<string | symbol>", "Future[str | Symbol]");
    });
    it("convert Iterator", () => {
      checkTypeToPython("Iterator<boolean>", "JsIterator[bool]");
      checkTypeToPython(
        "Iterator<boolean>",
        "PyGenerator[bool, None, Any]",
        Variance.contra,
      );
      checkTypeToPython(
        "Iterator<boolean, string, symbol>",
        "JsGenerator[bool, Symbol, str]",
      );
      checkTypeToPython(
        "Iterator<boolean, string, symbol>",
        "PyGenerator[bool, Symbol, str]",
        Variance.contra,
      );
    });
  });
  it("default type param", () => {
    const typeNode = getTypeNode("ReadableStream");
    const conversion = typeAstToString(typeNode, false, Variance.covar);
    assert.strictEqual(conversion, "ReadableStream[Any]");
  });
  describe("variance", () => {
    it("variance 1", () => {
      const typeNode = getTypeNode(
        "(a: Iterable<string>) => Iterable<boolean>;",
      );
      const conversion = removeTypeIgnores(
        typeAstToString(typeNode, false, Variance.covar, "myFunc"),
      );
      assert.strictEqual(
        conversion,
        "def myFunc(self, a: PyIterable[str], /) -> JsIterable[bool]: ...",
      );
    });
    it("variance 2", () => {
      const typeNode = getTypeNode(
        "(a: Iterable<IterableIterator<boolean>> ) => void;",
      );
      const conversion = removeTypeIgnores(
        typeAstToString(typeNode, false, Variance.covar, "myFunc"),
      );
      assert.strictEqual(
        conversion,
        "def myFunc(self, a: PyIterable[PyIterator[bool]], /) -> None: ...",
      );
    });
    it("variance 3", () => {
      const typeNode = getTypeNode(
        "(a: (b: Iterable<string>) => Iterable<boolean> ) => void;",
      );
      const conversion = removeTypeIgnores(
        typeAstToString(typeNode, false, Variance.covar, "myFunc"),
      );
      assert.strictEqual(
        conversion,
        "def myFunc(self, a: Callable[[JsIterable[str]], PyIterable[bool]], /) -> None: ...",
      );
    });
  });
  describe("callable types", () => {
    it("basic", () => {
      const typeNode = getTypeNode("() => void");
      const conversion = typeAstToString(typeNode, false, Variance.covar);
      assert.strictEqual(conversion, "Callable[[], None]");
    });
    it("toplevel", () => {
      const typeNode = getTypeNode("() => void");
      const conversion = removeTypeIgnores(
        typeAstToString(typeNode, false, Variance.covar, "myFunc"),
      );
      assert.strictEqual(conversion, "def myFunc(self, /) -> None: ...");
    });
    it("optional args", () => {
      const typeNode = getTypeNode("(a?: string) => void");
      const conversion = removeTypeIgnores(
        typeAstToString(typeNode, false, Variance.covar, "myFunc"),
      );
      assert.strictEqual(
        conversion,
        "def myFunc(self, a: str | None = None, /) -> None: ...",
      );
    });
    it("optional or null", () => {
      const typeNode = getTypeNode("(a?: string | null) => void;");
      const conversion = removeTypeIgnores(
        typeAstToString(typeNode, false, Variance.covar, "myFunc"),
      );
      assert.strictEqual(
        conversion,
        "def myFunc(self, a: str | None = None, /) -> None: ...",
      );
    });
    it("type predicate", () => {
      const typeNode = getTypeNode("(a: any) => a is string;");
      const conversion = removeTypeIgnores(
        typeAstToString(typeNode, false, Variance.covar, "myFunc"),
      );
      assert.strictEqual(
        conversion,
        "def myFunc(self, a: Any, /) -> bool: ...",
      );
    });
    it("dotdotdot arg", () => {
      const typeNode = getTypeNode("(...a: string[][]) => void;");
      const conversion = removeTypeIgnores(
        typeAstToString(typeNode, false, Variance.covar, "myFunc"),
      );
      assert.strictEqual(
        conversion,
        "def myFunc(self, /, *a: PyMutableSequence[str]) -> None: ...",
      );
    });
  });
});

describe("property signature", () => {
  it("mandatory function", () => {
    const fname = "/a.ts";
    const project = makeProject();
    project.createSourceFile(fname, `declare var X: {f: () => void};`);
    const file = project.getSourceFileOrThrow(fname);
    const [propsig] = file.getDescendantsOfKind(SyntaxKind.PropertySignature);
    const res = removeTypeIgnores(propertySignatureAstToString(propsig));
    assert.strictEqual(res, "def f(self, /) -> None: ...");
  });
  it("optional function", () => {
    const fname = "/a.ts";
    const project = makeProject();
    project.createSourceFile(fname, `declare var X: {f?: () => void};`);
    const file = project.getSourceFileOrThrow(fname);
    const [propsig] = file.getDescendantsOfKind(SyntaxKind.PropertySignature);
    const res = removeTypeIgnores(propertySignatureAstToString(propsig));
    assert.strictEqual(res, "f: Callable[[], None] | None = ...");
  });
  it("alternatives function", () => {
    const fname = "/a.ts";
    const project = makeProject();
    project.createSourceFile(
      fname,
      `declare var X: {f: (() => void) | string};`,
    );
    const file = project.getSourceFileOrThrow(fname);
    const [propsig] = file.getDescendantsOfKind(SyntaxKind.PropertySignature);
    const res = removeTypeIgnores(propertySignatureAstToString(propsig));
    assert.strictEqual(res, "f: (Callable[[], None]) | str = ...");
  });
  it("optional interface function", () => {
    const res = emitFile(`
      interface X {
          f? : () => void;
      }
      declare var Test: X[];
    `);
    assert.strictEqual(
      removeTypeIgnores(res.slice(1).join("\n\n")),
      dedent(`\
        Test: JsArray[X_iface] = ...

        class X_iface(Protocol):
            f: Callable[[], None] | None = ...
      `).trim(),
    );
  });
  it("alternatives interface function", () => {
    const res = emitFile(`
      interface X {
          f : (() => void) | string;
      }
      declare var Test: X[];
    `);
    assert.strictEqual(
      removeTypeIgnores(res.slice(1).join("\n\n")),
      dedent(`\
        Test: JsArray[X_iface] = ...

        class X_iface(Protocol):
            f: (Callable[[], None]) | str = ...
      `).trim(),
    );
  });
});

function convertVarDecl(astVarDecl: VariableDeclaration): string {
  const astConverter = new AstConverter();
  const irVarDecl = astConverter.varDeclToIR(astVarDecl)!;
  switch (irVarDecl.kind) {
    case "callable":
      return callableIRToString(irVarDecl, false).join("\n");
    case "declaration":
      return declarationIRToString(irVarDecl);
    case "interface":
      return interfaceIRToString(irVarDecl);
    case "typeAlias":
      return typeAliasIRToString(irVarDecl);
  }
}

function convertFuncDeclGroup(
  name: string,
  decls: FunctionDeclaration[],
): string {
  const astConverter = new AstConverter();
  const sigsIR = astConverter.funcDeclsToIR(name, decls);
  return callableIRToString(sigsIR, false).join("\n");
}

describe("sanitizeReservedWords", () => {
  it("variable name", () => {
    const project = makeProject();
    project.createSourceFile("/a.ts", "declare var global : string;");
    const file = project.getSourceFileOrThrow("/a.ts");
    const decl = file.getFirstDescendantByKind(SyntaxKind.VariableDeclaration)!;
    const res = removeTypeIgnores(convertVarDecl(decl));
    assert.strictEqual(res, "global_: str = ...");
  });
});

it("Constructor reference", () => {
  const text = `
    interface Test {}
    interface TestConstructor {
        new (): Test;
        readonly prototype: Test;
    }
    declare var Test: TestConstructor;
    `;
  const expected = dedent(`\
    class Test(Test_iface):
        @classmethod
        def new(self, /) -> Test: ...\
  `).trim();
  const project = makeProject();
  project.createSourceFile("/a.ts", text);
  const file = project.getSourceFileOrThrow("/a.ts");
  const decl = file.getFirstDescendantByKind(SyntaxKind.VariableDeclaration)!;
  const res = removeTypeIgnores(convertVarDecl(decl));
  assert.strictEqual(res, expected);
});

it("No args function", () => {
  const text = `
    declare function f(): void;
  `;
  const expected = dedent(`\
    def f() -> None: ...
  `).trim();
  const project = makeProject();
  project.createSourceFile("/a.ts", text);
  const file = project.getSourceFileOrThrow("/a.ts");
  const decl = file.getFirstDescendantByKind(SyntaxKind.FunctionDeclaration)!;
  const result = removeTypeIgnores(
    convertFuncDeclGroup(decl.getName()!, [decl]),
  );
  assert.strictEqual(result, expected);
});

function getBaseNames(
  defs: (InterfaceDeclaration | ClassDeclaration)[],
): string[] {
  const astConverter = new AstConverter();
  const bases = astConverter.getBasesOfDecls(defs);
  return bases.map((base) => baseIRToString(base));
}

describe("getBaseNames", () => {
  it("extends deduplication", () => {
    const text = `
      interface X {}
      interface S extends X {}
      interface S extends X {}
      `;
    const project = makeProject();
    project.createSourceFile("/a.ts", text);
    const file = project.getSourceFileOrThrow("/a.ts");
    const decls = file
      .getDescendantsOfKind(SyntaxKind.InterfaceDeclaration)
      .slice(1);
    assert.deepStrictEqual(getBaseNames(decls), ["X_iface"]);
  });

  it("type argument defaults", () => {
    const text = `
      interface X<Q = number> {}
      interface X<S = string> {}

      interface S1 extends X {}
      interface S2 extends X<boolean> {}
      interface S3 extends X<boolean, symbol> {}
      `;
    const project = makeProject();
    project.createSourceFile("/a.ts", text);
    const file = project.getSourceFileOrThrow("/a.ts");
    const decls = file.getDescendantsOfKind(SyntaxKind.InterfaceDeclaration)!;
    assert.strictEqual(
      getBaseNames([decls[2]])[0],
      "X_iface[int | float, str]",
    );
    assert.strictEqual(getBaseNames([decls[3]])[0], "X_iface[bool, str]");
    assert.strictEqual(getBaseNames([decls[4]])[0], "X_iface[bool, Symbol]");
  });
});

it("Type variable", () => {
  const text = `
    interface Test<T> {}
    interface TestConstructor {
        new<T> (): Test<T>;
        readonly prototype: Test;
    }
    declare var Test: TestConstructor;
    `;
  const expected = dedent(`
    class Test[T](Test_iface[T]):
        @classmethod
        def new(self, /) -> Test[T]: ...
  `).trim();
  const project = makeProject();
  project.createSourceFile("/a.ts", text);
  const file = project.getSourceFileOrThrow("/a.ts");
  const decl = file.getFirstDescendantByKind(SyntaxKind.VariableDeclaration)!;
  const res = removeTypeIgnores(convertVarDecl(decl));
  assert.strictEqual(res, expected);
});

function emitFile(text: string): string[] {
  const project = makeProject();
  project.createSourceFile("/a.ts", text);
  return emitFiles([project.getSourceFileOrThrow("/a.ts")]);
}

describe("emit", () => {
  describe("Basic conversions", () => {
    it("string type", () => {
      const res = emitFile("declare var a : string;");
      assert.strictEqual(removeTypeIgnores(res.at(-1)!), "a: str = ...");
    });
    it("number type", () => {
      const res = emitFile("declare var a : number;");
      assert.strictEqual(
        removeTypeIgnores(res.at(-1)!),
        "a: int | float = ...",
      );
    });
    it("boolean type", () => {
      const res = emitFile("declare var a : boolean;");
      assert.strictEqual(removeTypeIgnores(res.at(-1)!), "a: bool = ...");
    });
    it("extends", () => {
      const res = emitFile(`
        interface B {
            b: number;
        }

        interface A extends B {

        }

        declare var x: {
            a: A;
        };
      `);
      assert.strictEqual(
        removeTypeIgnores(
          res
            .slice(1)
            .filter((x) => x.trim())
            .join("\n\n"),
        ),
        dedent(`
          class x(_JsObject):
              a: ClassVar[A_iface] = ...

          class A_iface(B_iface, Protocol):
              pass

          class B_iface(Protocol):
              b: int | float = ...
        `).trim(),
      );
    });

    it("subclass with incompatible constructor type", () => {
      const res = emitFile(`
        interface Example {
            name: string;
        }

        interface ExampleConstructor {
            new (a?: string): Example;
            readonly prototype: Example;
        }

        declare var Example: ExampleConstructor;

        interface SubExample extends Example {
            field: string;
        }
        interface SubExampleConstructor {
        new (b?: boolean): SubExample;
        readonly prototype: SubExample;
        }
        declare var SubExample: SubExampleConstructor;
      `);
      assert.strictEqual(
        removeTypeIgnores(
          res
            .slice(1)
            .filter((x) => x.trim())
            .join("\n\n"),
        ),
        dedent(`
          class Example(Example_iface, _JsObject):
              @classmethod
              def new(self, a: str | None = None, /) -> Example: ...

          class SubExample(SubExample_iface, _JsObject):
              @classmethod
              def new(self, b: bool | None = None, /) -> SubExample: ...

          class Example_iface(Protocol):
              name: str = ...

          class SubExample_iface(Example_iface, Protocol):
              field: str = ...
        `).trim(),
      );
    });
  });

  it("type var", () => {
    const res = emitFile(`
      interface Test<T> {}
      interface TestConstructor {
          new<T> (): Test<T>;
          readonly prototype: Test;
      }
      declare var Test: TestConstructor;
    `);
    const expected = dedent(`
      class Test[T](Test_iface[T], _JsObject):
          @classmethod
          def new(self, /) -> Test[T]: ...

      class Test_iface[T](Protocol):
          pass
    `).trim();
    assert.strictEqual(
      removeTypeIgnores(
        res
          .slice(1)
          .filter((x) => x.trim())
          .join("\n\n"),
      ),
      expected,
    );
  });
  it("options param", () => {
    const res = emitFile(`
      interface XOptions {
        cause?: string;
      }

      interface X {}

      interface XConstructor {
        new (message?: string, options?: XOptions): X;
        readonly prototype: X;
      }

      declare var X: XConstructor;
    `);
    const expected = dedent(`
      class X(X_iface, _JsObject):
          @classmethod
          @overload
          def new(self, message: str | None = None, options: XOptions_iface | None = None, /) -> X: ...
          @classmethod
          @overload
          def new(self, message: str | None = None, /, *, cause: str | None = None) -> X: ...

      class X_iface(Protocol):
          pass

      class XOptions_iface(Protocol):
          cause: str | None = ...

    `).trim();
    assert.strictEqual(
      removeTypeIgnores(
        res
          .slice(1)
          .filter((x) => x.trim())
          .join("\n\n"),
      ),
      expected,
    );
  });
  it("MRO", () => {
    const res = emitFile(`
      interface A {}
      interface B extends A {}
      interface C extends A, B {}
      declare var c: { c: C };
    `);
    const expected = dedent(`
      class c(_JsObject):
          c: ClassVar[C_iface] = ...

      class C_iface(B_iface, A_iface, Protocol):
          pass

      class A_iface(Protocol):
          pass

      class B_iface(A_iface, Protocol):
          pass
    `).trim();
    assert.strictEqual(
      removeTypeIgnores(
        res
          .slice(1)
          .filter((x) => x.trim())
          .join("\n\n"),
      ),
      expected,
    );
  });
  it("concrete classes", () => {
    const res = emitFile(`
      interface A extends Error {}
      interface B extends A, Error {}
      interface C extends A, B, Error {}
      declare var c: { c: C };
    `);
    const expected = dedent(`
      class c(_JsObject):
          c: ClassVar[C_iface] = ...

      class C_iface(B_iface, A_iface, Error_iface):
          pass

      class A_iface(Error_iface):
          pass

      class B_iface(A_iface, Error_iface):
          pass

      class Error_iface(Exception):
          name: str = ...
          message: str = ...
          stack: str | None = ...
          cause: Any | None = ...
    `).trim();
    assert.strictEqual(
      removeTypeIgnores(
        res
          .slice(1)
          .filter((x) => x.trim())
          .join("\n\n"),
      ),
      expected,
    );
  });
  describe("Protocols", () => {
    it("Iterable", () => {
      const res = emitFile(`\
        interface X {
          [Symbol.iterator](): IterableIterator<string>;
        }
        declare var x: X[];
      `);
      assert.strictEqual(
        removeTypeIgnores(res.at(-1)!),
        dedent(`
          class X_iface(Protocol):
              def __iter__(self, /) -> PyIterator[str]: ...
        `).trim(),
      );
    });
    it("Sized", () => {
      const res = emitFile(`\
        interface X {
          length: number;
        }
        interface Y {
          size: number;
        }
        declare var x: X[];
        declare var y: Y[];
      `);
      assert.strictEqual(
        removeTypeIgnores(res.at(-2)!),
        dedent(`
          class X_iface(Protocol):
              length: int | float = ...
              def __len__(self, /) -> int: ...
        `).trim(),
      );
      assert.strictEqual(
        removeTypeIgnores(res.at(-1)!),
        dedent(`
          class Y_iface(Protocol):
              size: int | float = ...
              def __len__(self, /) -> int: ...
        `).trim(),
      );
    });
    it("contains", () => {
      const res = emitFile(`\
        interface X {
          includes(v: number): boolean;
        }
        interface Y {
          has(v: string): boolean;
        }
        declare var x: X[];
        declare var y: Y[];
      `);
      assert.strictEqual(
        removeTypeIgnores(res.at(-2)!),
        dedent(`
          class X_iface(Protocol):
              def includes(self, v: int | float, /) -> bool: ...
              def __contains__(self, v: int | float, /) -> bool: ...
        `).trim(),
      );
      assert.strictEqual(
        removeTypeIgnores(res.at(-1)!),
        dedent(`
          class Y_iface(Protocol):
              def has(self, v: str, /) -> bool: ...
              def __contains__(self, v: str, /) -> bool: ...
        `).trim(),
      );
    });
    it("map", () => {
      const res = emitFile(`\
        interface X {
          get(x: number): string;
          set(x: number, y: string): void;
          delete(x: number): boolean;
          has(x: number): boolean
        }
        declare var x: X[];
      `);
      assert.strictEqual(
        removeTypeIgnores(res.at(-1)!),
        dedent(`
          class X_iface(Protocol):
              def get(self, x: int | float, /) -> str: ...
              def set(self, x: int | float, y: str, /) -> None: ...
              def delete(self, x: int | float, /) -> bool: ...
              def has(self, x: int | float, /) -> bool: ...
              def __contains__(self, x: int | float, /) -> bool: ...
              def __getitem__(self, x: int | float, /) -> str: ...
              def __setitem__(self, x: int | float, y: str, /) -> None: ...
              def __delitem__(self, x: int | float, /) -> bool: ...
        `).trim(),
      );
    });
    it("callable interface", () => {
      const res = emitFile(`\
        declare var x: VoidFunction[];
      `);
      assert.strictEqual(
        removeTypeIgnores(res.at(-1)!),
        dedent(`
          class VoidFunction_iface(Protocol):
              def __call__(self, /) -> None: ...
        `).trim(),
      );
    });
  });
  it("duplicate signature", () => {
    const res = emitFile(`\
      declare function f(x: string): void;
      declare function f(y: string): void;
    `);
    assert.strictEqual(
      removeTypeIgnores(res.at(-1)!),
      "def f(x: str, /) -> None: ...",
    );
  });
  describe("extends string", () => {
    it("extends string ==> str in function type param", () => {
      const res = emitFile(`\
      declare function f<T extends string>(x: T): void;
    `);
      assert.strictEqual(
        removeTypeIgnores(res.at(-1)!),
        "def f(x: str, /) -> None: ...",
      );
    });
    it("extends string ==> str in interface", () => {
      const res = emitFile(`\
      declare interface X<K extends string> {
        f(k: K): void;
      }
      declare var x: X;
    `);
      assert.strictEqual(
        removeTypeIgnores(res.at(-1)!),
        dedent(`
        class x(_JsObject):
            @classmethod
            def f(self, k: str, /) -> None: ...
      `).trim(),
      );
    });
    it("extends string ==> str in interface 2", () => {
      const res = emitFile(`\
      declare interface X<K extends string> {
        f(k: K): void;
      }
      declare function g(x: X<string>): void;
    `);
      assert.strictEqual(
        removeTypeIgnores(res.at(-3)!),
        dedent(`
        @overload
        def g(x: X_iface, /) -> None: ...
      `).trim(),
      );
    });
    it("extends string class", () => {
      const res = emitFile(`\
      interface X<K extends string> {
      }
      interface XConstructor {
        readonly prototype: X;
      }
      declare var X: XConstructor;
    `);
      assert.strictEqual(
        removeTypeIgnores(res.slice(1).join("\n\n")),
        dedent(`
        class X(X_iface, _JsObject):
            pass

        class X_iface(Protocol):
            pass
      `).trim(),
      );
    });
  });
  describe("Type param", () => {
    it("Type alias param", () => {
      const res = emitFile(`\
      interface I<T> {
        t : T;
      }
      type F<T> = I<T> | number;
      declare function f(): F<string>;
    `);
      assert.strictEqual(
        removeTypeIgnores(res[1]),
        dedent(`
        type F[T] = I_iface[T] | int | float
      `).trim(),
      );
    });
    it("Function type param", () => {
      const res = emitFile(`\
      declare function f<T>(x: T): T;
    `);
      assert.strictEqual(
        removeTypeIgnores(res[1]),
        dedent(`
        def f[T](x: T, /) -> T: ...
      `).trim(),
      );
    });
    it("Interface method type param", () => {
      const res = emitFile(`\
      interface I {
        f<T>(x: T): T;
      }
      declare var x: I;
    `);
      assert.strictEqual(
        removeTypeIgnores(res[1]),
        dedent(`
        class x(_JsObject):
            @classmethod
            def f[T](self, x: T, /) -> T: ...
      `).trim(),
      );
    });
    it("some constructors have type params, others do not", () => {
      const res = emitFile(`\
      interface XIface<T> {
        x : T;
      }
      interface XConstructor {
          new (x?: number): XIface<any>;
          new <T>(x: number): XIface<T>;
      }
      declare var X: XConstructor;
    `);
      assert.strictEqual(
        removeTypeIgnores(res[1]),
        dedent(`
        class X(_JsObject):
            @classmethod
            @overload
            def new(self, x: int | float | None = None, /) -> XIface_iface[Any]: ...
            @classmethod
            @overload
            def new[T](self, x: int | float, /) -> XIface_iface[T]: ...
      `).trim(),
      );
    });
    it("Unpacking and type params", () => {
      const res = emitFile(`\
      interface XLike<T> {
        length: number;
        t: T;
      }

      interface XConstructor {
        from<T>(x: XLike<T>): T[];
      }

      declare var X: XConstructor;
    `);
      assert.strictEqual(
        removeTypeIgnores(res[1]),
        dedent(`
        class X(_JsObject):
            @classmethod
            @overload
            def from_[T](self, x: XLike_iface[T], /) -> JsArray[T]: ...
            @classmethod
            @overload
            def from_[T](self, /, *, length: int | float, t: T) -> JsArray[T]: ...
      `).trim(),
      );
    });
    it("Unpacking and type params 2", () => {
      const res = emitFile(`\
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
      assert.strictEqual(
        removeTypeIgnores(res[1]),
        dedent(`
        class X(_JsObject):
            @classmethod
            @overload
            def new[R](self, source: R, strategy: S_iface[R] | None = None, /) -> X[R]: ...
            @classmethod
            @overload
            def new[R](self, source: R, /, *, t: R) -> X[R]: ...
      `).trim(),
      );
    });
    it("Unpacking and type params 3", () => {
      const res = emitFile(`\
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
      assert.strictEqual(
        removeTypeIgnores(res[1]),
        dedent(`
        class X(_JsObject):
            @classmethod
            @overload
            def new[R](self, source: R, strategy: S_iface[str] | None = None, /) -> X[R]: ...
            @classmethod
            @overload
            def new[R](self, source: R, /, *, t: str) -> X[R]: ...
      `).trim(),
      );
    });
    it("Unpacking and type params 4", () => {
      const res = emitFile(`\
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
      assert.strictEqual(
        removeTypeIgnores(res[1]),
        dedent(`
        class X(_JsObject):
            @classmethod
            @overload
            def new[R](self, source: R, strategy: S_iface[R] | None = None, /) -> Xiface_iface[R]: ...
            @classmethod
            @overload
            def new[R](self, source: R, /, *, t: R, s: Size_iface[R]) -> Xiface_iface[R]: ...
      `).trim(),
      );
    });
    it("Unpacking and type params 5", () => {
      const res = emitFile(`\
      interface Strategy<S> {
        s: S;
      }

      interface Options<T> {
        s: Strategy<T>
      }

      interface Xyz<R = any> {
        r: R;
      }

      declare var Xyz: {
        prototype: Xyz;
        new<R = any>(strategy?: Options<R>): Xyz<R>;
      };
    `);
      assert.strictEqual(
        removeTypeIgnores(res[1]),
        dedent(`
        class Xyz[R](Xyz_iface[R], _JsObject):
            @classmethod
            @overload
            def new(self, strategy: Options_iface[R] | None = None, /) -> Xyz[R]: ...
            @classmethod
            @overload
            def new(self, /, *, s: Strategy_iface[R]) -> Xyz[R]: ...
      `).trim(),
      );
    });
    it("Type alias type param with default value", () => {
      const res = emitFile(`\
      interface X<A, B> {
        a: A;
        b: B;
      };
      type Info<A = string, B = number> =
        | X<A, B>
        | string;

      declare function f(x: Info): void;
    `);
      assert.strictEqual(
        removeTypeIgnores(res[2]),
        "def f(x: Info[str, int | float], /) -> None: ...",
      );
    });
    it("Type param default refers to other type param", () => {
      const res = emitFile(`
      interface X<T> {
        t: T;
      }
      type Info<A = string, B = X<A>> =
        | A
        | B;
      declare function f(x: Info): void;
    `);
      assert.strictEqual(
        removeTypeIgnores(res[2]),
        "def f(x: Info[str, X_iface[str]], /) -> None: ...",
      );
    });
    it("type param default in class property", () => {
      const res = emitFile(`
      declare class T<M = string> {
        m: M;
      }
      declare class E {
        readonly t?: T;
      }
    `);
      assert.strictEqual(
        removeTypeIgnores(res.slice(1).join("\n\n")),
        dedent(`\
        class T_iface[M](Protocol):
            m: M = ...

        class T[M](T_iface[M], _JsObject):
            pass

        class E_iface(Protocol):
            @property
            def t(self, /) -> T[str] | None: ...

        class E(E_iface, _JsObject):
            pass
      `).trim(),
      );
    });
  });
  it("Array converted to ArrayLike_iface", () => {
    const res = emitFile(`declare function f(x: Array<string>): void`);
    assert.strictEqual(
      removeTypeIgnores(res.slice(1).join("\n\n")),
      "def f(x: ArrayLike_iface[str], /) -> None: ...",
    );
  });
  it("extends record", () => {
    const res = emitFile(`
      interface I extends Record<string, number> {
        x: string;
      }
      declare function f(i: I): void;
    `);
    assert.strictEqual(
      removeTypeIgnores(res.slice(1).join("\n\n")),
      dedent(`
        @overload
        def f(i: I_iface, /) -> None: ...

        @overload
        def f(*, x: str) -> None: ...

        class I_iface(Protocol):
            x: str = ...
            def __getattr__(self, key: str, /) -> int | float: ...
      `).trim(),
    );
  });
  it("extends Iterator", () => {
    const res = emitFile(`
      interface IteratorObject<T, TReturn = unknown, TNext = unknown> extends Iterator<T, TReturn, TNext> {
          [Symbol.iterator](): IteratorObject<T, TReturn, TNext>;
      }
      declare function f(): IteratorObject<string, undefined, undefined>'
    `);
    assert.strictEqual(
      removeTypeIgnores(res.slice(1).join("\n\n")),
      dedent(`
        def f() -> IteratorObject_iface[str, None, None]: ...

        class IteratorObject_iface[T, TReturn, TNext](JsGenerator[T, TNext, TReturn]):
            def __iter__(self, /) -> PyIterator[T]: ...
      `).trim(),
    );
  });
  describe("typescript classes", () => {
    it("simple class", () => {
      const res = emitFile(`
        declare class X {
          x: string;
          constructor(y: string);
        }
      `);
      assert.strictEqual(
        removeTypeIgnores(res.slice(1).join("\n\n")),
        dedent(`
          class X_iface(Protocol):
              x: str = ...

          class X(X_iface, _JsObject):
              @classmethod
              def new(self, y: str, /) -> X_iface: ...
        `).trim(),
      );
    });
    it("class with methods", () => {
      const res = emitFile(`
        declare class X {
          x: string;
          constructor(y: string);
          static f(x: string): number;
          f(x: number): string;
        }
      `);
      assert.strictEqual(
        removeTypeIgnores(res.slice(1).join("\n\n")),
        dedent(`
          class X_iface(Protocol):
              x: str = ...
              def f(self, x: int | float, /) -> str: ...

          class X(X_iface, _JsObject):
              @classmethod
              def f(self, x: str, /) -> int | float: ...
              @classmethod
              def new(self, y: str, /) -> X_iface: ...
        `).trim(),
      );
    });
    it("class with type parameter", () => {
      const res = emitFile(`
        declare class X<T> {
          x: T;
          constructor(y: T);
          static f<T>(x: T): X<T>;
          f(x: number): T;
        }
      `);
      assert.strictEqual(
        removeTypeIgnores(res.slice(1).join("\n\n")),
        dedent(`
          class X_iface[T](Protocol):
              x: T = ...
              def f(self, x: int | float, /) -> T: ...

          class X[T](X_iface[T], _JsObject):
              @classmethod
              def f(self, x: T, /) -> X[T]: ...
              @classmethod
              def new(self, y: T, /) -> X_iface[T]: ...
        `).trim(),
      );
    });
    describe("Class protocols", () => {
      it("Iterable", () => {
        const res = emitFile(`\
          class X {
            [Symbol.iterator](): IterableIterator<string> {}
          }
        `);
        assert.strictEqual(
          removeTypeIgnores(res.at(-2)!),
          dedent(`
            class X_iface(Protocol):
                def __iter__(self, /) -> PyIterator[str]: ...
          `).trim(),
        );
      });
      it("Sized", () => {
        const res = emitFile(`\
          class X {
            length: number;
          }
          class Y {
            size: number;
          }
        `);
        assert.strictEqual(
          removeTypeIgnores(res.at(-4)!),
          dedent(`
            class X_iface(Protocol):
                length: int | float = ...
                def __len__(self, /) -> int: ...
          `).trim(),
        );
        assert.strictEqual(
          removeTypeIgnores(res.at(-2)!),
          dedent(`
            class Y_iface(Protocol):
                size: int | float = ...
                def __len__(self, /) -> int: ...
          `).trim(),
        );
      });
      it("contains", () => {
        const res = emitFile(`\
          class X {
            includes(v: number): boolean {};
          }
          class Y {
            has(v: string): boolean {};
          }
        `);
        assert.strictEqual(
          removeTypeIgnores(res.at(-4)!),
          dedent(`
            class X_iface(Protocol):
                def includes(self, v: int | float, /) -> bool: ...
                def __contains__(self, v: int | float, /) -> bool: ...
          `).trim(),
        );
        assert.strictEqual(
          removeTypeIgnores(res.at(-2)!),
          dedent(`
            class Y_iface(Protocol):
                def has(self, v: str, /) -> bool: ...
                def __contains__(self, v: str, /) -> bool: ...
          `).trim(),
        );
      });
    });
  });
  describe("destructuring", () => {
    it("Destrucure object literal argument", () => {
      const res = emitFile(`
        declare function f({
            type,
            payload,
          }: {
            type: string;
            payload: number;
          }): void;
      `);
      assert.strictEqual(
        removeTypeIgnores(res.slice(1).join("\n\n")),
        "def f(*, type: str, payload: int | float) -> None: ...",
      );
    });
    it("Destructure name collision", () => {
      const res = emitFile(`
        declare function f(type: string, options: {
            type: string;
            payload: number;
          }): void;
      `);
      assert.strictEqual(
        removeTypeIgnores(res.slice(1).join("\n\n")),
        "def f(type_: str, /, *, type: str, payload: int | float) -> None: ...",
      );
    });
  });
  describe("Type literals", () => {
    it("simple", () => {
      const res = emitFile(`
        type A = {
            a?: 1 | 2 | 3 | 4 | 5;
            b?: number;
            c?: 'a' | 'b' | 'c';
        };
        declare function f(): A;
      `);
      assert.strictEqual(
        removeTypeIgnores(res.slice(1).join("\n\n")),
        dedent(`
          type A = A_iface

          def f() -> A: ...

          class A_iface(Protocol):
              a: Literal[1, 2, 3, 4, 5] | None = ...
              b: int | float | None = ...
              c: Literal['a', 'b', 'c'] | None = ...
        `).trim(),
      );
    });
    it("union", () => {
      const res = emitFile(`
        type A = { a: number } | { b: string };
        declare function f(): A;
      `);
      assert.strictEqual(
        removeTypeIgnores(res.slice(1).join("\n\n")),
        dedent(`
          type A = A__Union0_iface | A__Union1_iface

          def f() -> A: ...

          class A__Union0_iface(Protocol):
              a: int | float = ...

          class A__Union1_iface(Protocol):
              b: str = ...
        `).trim(),
      );
    });
    it("nested", () => {
      const res = emitFile(`
        type A = { a: number; b: { c: number; }; };
        declare function f(): A;
      `);
      assert.strictEqual(
        removeTypeIgnores(res.slice(1).join("\n\n")),
        dedent(`
          type A = A_iface

          def f() -> A: ...

          class A__b_iface(Protocol):
              c: int | float = ...

          class A_iface(Protocol):
              a: int | float = ...
              b: A__b_iface = ...
        `).trim(),
      );
    });
    it("intersection", () => {
      const res = emitFile(`
        type A = { a: number } & { b: string };
        declare function f(): A;
      `);
      assert.strictEqual(
        removeTypeIgnores(res.slice(1).join("\n\n")),
        dedent(`
          type A = A_iface

          def f() -> A: ...

          class A__Intersection0_iface(Protocol):
              a: int | float = ...

          class A__Intersection1_iface(Protocol):
              b: str = ...

          class A_iface(A__Intersection1_iface, A__Intersection0_iface, Protocol):
              pass
        `).trim(),
      );
    });
    it("intersection2", () => {
      const res = emitFile(`
        type F = {
            a: string;
        };
        type D = F & {
            readonly id: number;
        };
        declare function f(): D;
      `);
      assert.strictEqual(
        removeTypeIgnores(res.slice(1).join("\n\n")),
        dedent(`
          type D = D_iface

          type F = F_iface

          def f() -> D: ...

          class D__Intersection1_iface(Protocol):
              @property
              def id(self, /) -> int | float: ...

          class D_iface(F_iface, D__Intersection1_iface, Protocol):
              pass

          class F_iface(Protocol):
              a: str = ...
        `).trim(),
      );
    });
  });
  describe("Type operators", () => {
    describe("Omit", () => {
      it("OmitLiteral1", () => {
        const res = emitFile(`
          type D = Omit<{ a: string; b: number; }, "b">;
          declare function f(): D;
        `);
        assert.strictEqual(
          removeTypeIgnores(res.slice(1).join("\n\n")),
          dedent(`
            type D = D__Omit_iface

            def f() -> D: ...

            class D__Omit_iface(Protocol):
                a: str = ...
          `).trim(),
        );
      });
      it("OmitLiteral2", () => {
        const res = emitFile(`
          type D = Omit<{ a: string; b: string; c: number; }, "b" | "c">;
          declare function f(): D;
        `);
        assert.strictEqual(
          removeTypeIgnores(res.slice(1).join("\n\n")),
          dedent(`
            type D = D__Omit_iface

            def f() -> D: ...

            class D__Omit_iface(Protocol):
                a: str = ...
          `).trim(),
        );
      });
      it("OmitIntersection", () => {
        const res = emitFile(`
          type D = Omit<{ a: string; b: string; } & { c : string; }, "b">;
          declare function f(): D;
        `);
        // Not ideal generation here...
        assert.strictEqual(
          removeTypeIgnores(res.slice(1).join("\n\n")),
          dedent(`
            type D = D__Omit_iface

            def f() -> D: ...

            class D__Omit__Intersection0_iface(Protocol):
                a: str = ...

            class D__Omit__Intersection1_iface(Protocol):
                c: str = ...

            class D__Omit_iface(D__Omit__Intersection1_iface, D__Omit__Intersection0_iface, Protocol):
                pass
          `).trim(),
        );
      });
      it("OmitInterface", () => {
        const res = emitFile(`
          interface A { a: string; b: string; }
          type D = Omit<A, "b">;
          declare function f(): D;
        `);
        assert.strictEqual(
          removeTypeIgnores(res.slice(1).join("\n\n")),
          dedent(`
            type D = D__Omit__A_iface

            def f() -> D: ...

            class D__Omit__A_iface(Protocol):
                a: str = ...
          `).trim(),
        );
      });
      it("OmitAlias", () => {
        const res = emitFile(`
          type A = {
              s?: boolean;
              t?: string;
          };
          type B = Omit<A, 's'>;
          declare function f(): B;
        `);
        assert.strictEqual(
          removeTypeIgnores(res.slice(1).join("\n\n")),
          dedent(`
            type B = B__Omit_iface

            def f() -> B: ...

            class B__Omit_iface(Protocol):
                t: str | None = ...
          `).trim(),
        );
      });
      it("Omit NoOp", () => {
        const res = emitFile(`
          type A = {
              s?: boolean;
              t?: string;
          };
          type B = Omit<A, 'x'>;
          declare function f(a: A, b: B): void;
        `);
        assert.strictEqual(
          removeTypeIgnores(res.slice(1).join("\n\n")),
          dedent(`
            type A = A_iface

            type B = A

            def f(a: A, b: B, /) -> None: ...

            class A_iface(Protocol):
                s: bool | None = ...
                t: str | None = ...
          `).trim(),
        );
      });
    });
  });
  describe("adjustments", () => {
    it("setTimeout", () => {
      const res = emitIRNoTypeIgnores(convertBuiltinFunction("setTimeout"));
      assert.strictEqual(
        res.at(-1),
        "def setTimeout(handler: TimerHandler, timeout: int | float | None = None, /, *arguments: Any) -> int | JsProxy: ...",
      );
    });
    it("clearTimeout", () => {
      const res = emitIRNoTypeIgnores(convertBuiltinFunction("clearTimeout"));
      assert.strictEqual(
        res.at(-1),
        "def clearTimeout(id: int | JsProxy, /) -> None: ...",
      );
    });
  });
});

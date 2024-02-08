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
import { emitFiles, emitIR } from "../extract.ts";
import {
  propertyIRToString,
  baseIRToString,
  callableIRToString,
  topLevelIRToString,
  typeIRToString,
} from "../irToString.ts";
import { Variance } from "../types.ts";
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
} from "../astToIR";

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
  expect(conversion).toBe(pyType);
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
  return convertDecls([], [funcDecl]);
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
  const varDecl = id.getDefinitionNodes().filter(Node.isVariableDeclaration)[0];
  const ir = convertDecls([varDecl], []);
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
      checkTypeToPython("Exclude<string | symbol>", "str | Symbol");
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
    expect(conversion).toBe("ReadableStream[Any]");
  });
  describe("variance", () => {
    it("variance 1", () => {
      const typeNode = getTypeNode(
        "(a: Iterable<string>) => Iterable<boolean>;",
      );
      const conversion = removeTypeIgnores(
        typeAstToString(typeNode, false, Variance.covar, "myFunc"),
      );
      expect(conversion).toBe(
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
      expect(conversion).toBe(
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
      expect(conversion).toBe(
        "def myFunc(self, a: Callable[[JsIterable[str]], PyIterable[bool]], /) -> None: ...",
      );
    });
  });
  describe("callable types", () => {
    it("basic", () => {
      const typeNode = getTypeNode("() => void");
      const conversion = typeAstToString(typeNode, false, Variance.covar);
      expect(conversion).toBe("Callable[[], None]");
    });
    it("toplevel", () => {
      const typeNode = getTypeNode("() => void");
      const conversion = removeTypeIgnores(
        typeAstToString(typeNode, false, Variance.covar, "myFunc"),
      );
      expect(conversion).toBe("def myFunc(self, /) -> None: ...");
    });
    it("optional args", () => {
      const typeNode = getTypeNode("(a?: string) => void");
      const conversion = removeTypeIgnores(
        typeAstToString(typeNode, false, Variance.covar, "myFunc"),
      );
      expect(conversion).toBe(
        "def myFunc(self, a: str | None = None, /) -> None: ...",
      );
    });
    it("optional or null", () => {
      const typeNode = getTypeNode("(a?: string | null) => void;");
      const conversion = removeTypeIgnores(
        typeAstToString(typeNode, false, Variance.covar, "myFunc"),
      );
      expect(conversion).toBe(
        "def myFunc(self, a: str | None = None, /) -> None: ...",
      );
    });
    it("type predicate", () => {
      const typeNode = getTypeNode("(a: any) => a is string;");
      const conversion = removeTypeIgnores(
        typeAstToString(typeNode, false, Variance.covar, "myFunc"),
      );
      expect(conversion).toBe("def myFunc(self, a: Any, /) -> bool: ...");
    });
    it("dotdotdot arg", () => {
      const typeNode = getTypeNode("(...a: string[][]) => void;");
      const conversion = removeTypeIgnores(
        typeAstToString(typeNode, false, Variance.covar, "myFunc"),
      );
      expect(conversion).toBe(
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
    expect(res).toBe("def f(self, /) -> None: ...");
  });
  it("optional function", () => {
    const fname = "/a.ts";
    const project = makeProject();
    project.createSourceFile(fname, `declare var X: {f?: () => void};`);
    const file = project.getSourceFileOrThrow(fname);
    const [propsig] = file.getDescendantsOfKind(SyntaxKind.PropertySignature);
    const res = removeTypeIgnores(propertySignatureAstToString(propsig));
    expect(res).toBe("f: Callable[[], None] | None = ...");
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
    expect(res).toBe("f: (Callable[[], None]) | str = ...");
  });
  it("optional interface function", () => {
    const res = emitFile(`
      interface X {
          f? : () => void;
      }
      declare var Test: X[];
    `);
    expect(removeTypeIgnores(res.slice(2).join("\n\n"))).toBe(
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
    expect(removeTypeIgnores(res.slice(2).join("\n\n"))).toBe(
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
  const irVarDecl = astConverter.varDeclToIR(astVarDecl);
  return topLevelIRToString(irVarDecl);
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
    const decl = file.getFirstDescendantByKind(SyntaxKind.VariableDeclaration);
    const res = removeTypeIgnores(convertVarDecl(decl));
    expect(res).toBe("global_: str = ...");
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
  const decl = file.getFirstDescendantByKind(SyntaxKind.VariableDeclaration);
  const res = removeTypeIgnores(convertVarDecl(decl));
  expect(res).toBe(expected);
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
  const decl = file.getFirstDescendantByKind(SyntaxKind.FunctionDeclaration);
  const result = removeTypeIgnores(
    convertFuncDeclGroup(decl.getName(), [decl]),
  );
  expect(result).toBe(expected);
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
    expect(getBaseNames(decls)).toStrictEqual(["X_iface"]);
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
    const decls = file.getDescendantsOfKind(SyntaxKind.InterfaceDeclaration);
    expect(getBaseNames([decls[2]])[0]).toBe("X_iface[int | float, str]");
    expect(getBaseNames([decls[3]])[0]).toBe("X_iface[bool, str]");
    expect(getBaseNames([decls[4]])[0]).toBe("X_iface[bool, Symbol]");
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
    class Test(Test_iface[T]):
        @classmethod
        def new(self, /) -> Test[T]: ...
  `).trim();
  const project = makeProject();
  project.createSourceFile("/a.ts", text);
  const file = project.getSourceFileOrThrow("/a.ts");
  const decl = file.getFirstDescendantByKind(SyntaxKind.VariableDeclaration);
  const res = removeTypeIgnores(convertVarDecl(decl));
  expect(res).toBe(expected);
});

function emitFile(text) {
  const project = makeProject();
  project.createSourceFile("/a.ts", text);
  return emitFiles([project.getSourceFileOrThrow("/a.ts")]);
}

describe("emit", () => {
  describe("Basic conversions", () => {
    it("string type", () => {
      const res = emitFile("declare var a : string;");
      expect(removeTypeIgnores(res.at(-1))).toBe("a: str = ...");
    });
    it("number type", () => {
      const res = emitFile("declare var a : number;");
      expect(removeTypeIgnores(res.at(-1))).toBe("a: int | float = ...");
    });
    it("boolean type", () => {
      const res = emitFile("declare var a : boolean;");
      expect(removeTypeIgnores(res.at(-1))).toBe("a: bool = ...");
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
      expect(
        removeTypeIgnores(
          res
            .slice(1)
            .filter((x) => x.trim())
            .join("\n\n"),
        ),
      ).toEqual(
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
      expect(
        removeTypeIgnores(
          res
            .slice(1)
            .filter((x) => x.trim())
            .join("\n\n"),
        ),
      ).toEqual(
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
      T = TypeVar("T")

      class Test(Test_iface[T], _JsObject):
          @classmethod
          def new(self, /) -> Test[T]: ...

      class Test_iface(Generic[T], Protocol):
          pass
    `).trim();
    expect(
      removeTypeIgnores(
        res
          .slice(1)
          .filter((x) => x.trim())
          .join("\n\n"),
      ),
    ).toEqual(expected);
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
    expect(
      removeTypeIgnores(
        res
          .slice(1)
          .filter((x) => x.trim())
          .join("\n\n"),
      ),
    ).toEqual(expected);
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
    expect(
      removeTypeIgnores(
        res
          .slice(1)
          .filter((x) => x.trim())
          .join("\n\n"),
      ),
    ).toEqual(expected);
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
    expect(
      removeTypeIgnores(
        res
          .slice(1)
          .filter((x) => x.trim())
          .join("\n\n"),
      ),
    ).toEqual(expected);
  });
  it("Iterable", () => {
    const res = emitFile(`\
      interface X {
        [Symbol.iterator](): IterableIterator<string>;
      }
      declare var x: X[];
    `);
    expect(removeTypeIgnores(res.at(-1))).toBe(
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
    expect(removeTypeIgnores(res.at(-2))).toBe(
      dedent(`
        class X_iface(Protocol):
            length: int | float = ...
            def __len__(self, /) -> int: ...
      `).trim(),
    );
    expect(removeTypeIgnores(res.at(-1))).toBe(
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
    expect(removeTypeIgnores(res.at(-2))).toBe(
      dedent(`
        class X_iface(Protocol):
            def includes(self, v: int | float, /) -> bool: ...
            def __contains__(self, v: int | float, /) -> bool: ...
      `).trim(),
    );
    expect(removeTypeIgnores(res.at(-1))).toBe(
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
    expect(removeTypeIgnores(res.at(-1))).toBe(
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
    expect(removeTypeIgnores(res.at(-1))).toBe(
      dedent(`
        class VoidFunction_iface(Protocol):
            def __call__(self, /) -> None: ...
      `).trim(),
    );
  });
  it("duplicate signature", () => {
    const res = emitFile(`\
      declare function f(x: string): void;
      declare function f(y: string): void;
    `);
    expect(removeTypeIgnores(res.at(-1))).toBe("def f(x: str, /) -> None: ...");
  });
  describe("adjustments", () => {
    it("setTimeout", () => {
      const res = emitIRNoTypeIgnores(convertBuiltinFunction("setTimeout"));
      expect(res.at(-2)).toBe(
        "def setTimeout(handler: TimerHandler, timeout: int | float | None = None, /, *arguments: Any) -> int | JsProxy: ...",
      );
    });
    it("clearTimeout", () => {
      const res = emitIRNoTypeIgnores(convertBuiltinFunction("clearTimeout"));
      expect(res.at(-1)).toBe(
        "def clearTimeout(id: int | JsProxy, /) -> None: ...",
      );
    });
  });
});

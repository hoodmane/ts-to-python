import { SyntaxKind } from "ts-morph";
import { Converter } from "../extract.ts";
import { renderPyClass } from "../render.ts";
import { PyClass, PyOther } from "../types.ts";

let n = 0;
function getTypeNode(converter: Converter, type) {
  const fname = `/getTypeNode_$${n}.ts`;
  converter.project.createSourceFile(fname, `type A = ${type};`);
  const file = converter.project.getSourceFileOrThrow(fname);
  const alias = file.getFirstDescendantByKind(SyntaxKind.TypeAliasDeclaration);
  return alias.getTypeNode();
}

function removeTypeIgnores(a: string) {
  return a.replaceAll(/\s*#.*$/gm, "");
}

function dedent(s) {
  const lines = s.split("\n");
  let numSpaces = Infinity;
  for (const line of lines) {
    if (/^\s*$/.test(line)) {
      continue;
    }
    numSpaces = Math.min(numSpaces, /^\s*/.exec(line)[0].length);
  }
  return lines
    .map((line) => {
      if (/^\s*$/.test(line)) {
        return "";
      }
      return line.slice(numSpaces);
    })
    .join("\n");
}

describe("unit types", () => {
  it("convert string", () => {
    const converter = new Converter();
    const typeNode = getTypeNode(converter, "string");
    const conversion = converter.typeToPython(typeNode, false);
    expect(conversion).toBe("str");
  });
  it("convert number", () => {
    const converter = new Converter();
    const typeNode = getTypeNode(converter, "number");
    const conversion = converter.typeToPython(typeNode, false);
    expect(conversion).toBe("int | float");
  });
  it("convert union", () => {
    const converter = new Converter();
    const typeNode = getTypeNode(converter, "string | boolean");
    const conversion = converter.typeToPython(typeNode, false);
    expect(conversion).toBe("str | bool");
  });
  it("default type param", () => {
    const converter = new Converter();
    const typeNode = getTypeNode(converter, "ReadableStream");
    const conversion = converter.typeToPython(typeNode, false);
    expect(conversion).toBe("ReadableStream[Any]");
  });
  describe("callable types", () => {
    it("basic", () => {
      const converter = new Converter();
      const typeNode = getTypeNode(converter, "() => void");
      const conversion = converter.typeToPython(typeNode, false);
      expect(conversion).toBe("Callable[[], None]");
    });
    it("toplevel", () => {
      const converter = new Converter();
      const typeNode = getTypeNode(converter, "() => void");
      const conversion = removeTypeIgnores(
        converter.typeToPython(typeNode, false, "myFunc"),
      );
      expect(conversion).toBe("def myFunc(self, /) -> None: ...");
    });
    it("optional args", () => {
      const converter = new Converter();
      const typeNode = getTypeNode(converter, "(a?: string) => void");
      const conversion = removeTypeIgnores(
        converter.typeToPython(typeNode, false, "myFunc"),
      );
      expect(conversion).toBe(
        "def myFunc(self, a: str | None = None, /) -> None: ...",
      );
    });
    it("optional or null", () => {
      const converter = new Converter();
      const typeNode = getTypeNode(converter, "(a?: string | null) => void;");
      const conversion = removeTypeIgnores(
        converter.typeToPython(typeNode, false, "myFunc"),
      );
      expect(conversion).toBe(
        "def myFunc(self, a: str | None = None, /) -> None: ...",
      );
    });
    it("type predicate", () => {
      const converter = new Converter();
      const typeNode = getTypeNode(converter, "(a: any) => a is string;");
      const conversion = removeTypeIgnores(
        converter.typeToPython(typeNode, false, "myFunc"),
      );
      expect(conversion).toBe("def myFunc(self, a: Any, /) -> bool: ...");
    });
    it("dotdotdot arg", () => {
      const converter = new Converter();
      const typeNode = getTypeNode(converter, "(...a: string[][]) => void;");
      const conversion = removeTypeIgnores(
        converter.typeToPython(typeNode, false, "myFunc"),
      );
      expect(conversion).toBe(
        "def myFunc(self, /, *a: list[JsArray[str]]) -> None: ...",
      );
    });
  });
});

describe("property signature", () => {
  it("mandatory function", () => {
    const fname = "/a.ts";
    const converter = new Converter();
    converter.project.createSourceFile(
      fname,
      `
            declare var X: {f: () => void};
        `,
    );
    const file = converter.project.getSourceFileOrThrow(fname);
    const [propsig] = file.getDescendantsOfKind(SyntaxKind.PropertySignature);
    const res = removeTypeIgnores(converter.convertPropertySignature(propsig));
    expect(res).toBe("def f(self, /) -> None: ...");
  });
  it("optional function", () => {
    const fname = "/a.ts";
    const converter = new Converter();
    converter.project.createSourceFile(
      fname,
      `
            declare var X: {f?: () => void};
        `,
    );
    const file = converter.project.getSourceFileOrThrow(fname);
    const [propsig] = file.getDescendantsOfKind(SyntaxKind.PropertySignature);
    const res = removeTypeIgnores(converter.convertPropertySignature(propsig));
    expect(res).toBe("f: Callable[[], None] | None = ...");
  });
});

describe("sanitizeReservedWords", () => {
  it("variable name", () => {
    const converter = new Converter();
    converter.project.createSourceFile("/a.ts", "declare var global : string;");
    const file = converter.project.getSourceFileOrThrow("/a.ts");
    const decl = file.getFirstDescendantByKind(SyntaxKind.VariableDeclaration);
    const res = removeTypeIgnores(
      (converter.convertVarDecl(decl) as PyOther).text,
    );
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
  const converter = new Converter();
  converter.project.createSourceFile("/a.ts", text);
  const file = converter.project.getSourceFileOrThrow("/a.ts");
  const decl = file.getFirstDescendantByKind(SyntaxKind.VariableDeclaration);
  const cls = converter.convertVarDecl(decl) as PyClass;
  const res = removeTypeIgnores(renderPyClass(cls));
  expect(res).toBe(expected);
});

describe("getBaseNames", () => {
  it("extends deduplcation", () => {
    const text = `
      interface X {}
      interface S extends X {}
      interface S extends X {}
      `;
    const converter = new Converter();
    converter.project.createSourceFile("/a.ts", text);
    const file = converter.project.getSourceFileOrThrow("/a.ts");
    const decls = file
      .getDescendantsOfKind(SyntaxKind.InterfaceDeclaration)
      .slice(1);
    expect(converter.getBaseNames(decls)).toStrictEqual(["X_iface"]);
  });

  it("type argument defaults", () => {
    const text = `
      interface X<Q = number> {}
      interface X<S = string> {}

      interface S1 extends X {}
      interface S2 extends X<boolean> {}
      interface S3 extends X<boolean, symbol> {}
      `;
    const converter = new Converter();
    converter.project.createSourceFile("/a.ts", text);
    const file = converter.project.getSourceFileOrThrow("/a.ts");
    const decls = file.getDescendantsOfKind(SyntaxKind.InterfaceDeclaration);
    expect(converter.getBaseNames([decls[2]])[0]).toBe(
      "X_iface[int | float, str]",
    );
    expect(converter.getBaseNames([decls[3]])[0]).toBe("X_iface[bool, str]");
    expect(converter.getBaseNames([decls[4]])[0]).toBe("X_iface[bool, Symbol]");
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
  const converter = new Converter();
  converter.project.createSourceFile("/a.ts", text);
  const file = converter.project.getSourceFileOrThrow("/a.ts");
  const decl = file.getFirstDescendantByKind(SyntaxKind.VariableDeclaration);
  const cls = converter.convertVarDecl(decl) as PyClass;
  const res = removeTypeIgnores(renderPyClass(cls));
  expect(res).toBe(expected);
});

function emitFile(text) {
  const converter = new Converter();
  converter.project.createSourceFile("/a.ts", text);
  return converter.emit([converter.project.getSourceFileOrThrow("/a.ts")]);
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
          class x:
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
          class Example(Example_iface):
              @classmethod
              def new(self, a: str | None = None, /) -> Example: ...

          class SubExample(SubExample_iface):
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

      class Test(Test_iface[T]):
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
        cause?: unknown;
      }

      interface X {}

      interface XConstructor {
        new (message?: string, options?: XOptions): X;
        readonly prototype: X;
      }

      declare var X: XConstructor;
    `);
    const expected = dedent(`
      class X(X_iface):
          @classmethod
          def new(self, message: str | None = None, options: XOptions_iface | None = None, /) -> X: ...

      class X_iface(Protocol):
          pass

      class XOptions_iface(Protocol):
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
  it("MRO", () => {
    const res = emitFile(`
      interface A {}
      interface B extends A {}
      interface C extends A, B {}
      declare var c: { c: C };
    `);
    const expected = dedent(`
      class c:
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
      class c:
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
});

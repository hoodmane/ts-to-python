import { SyntaxKind } from "ts-morph";
import { Converter } from "../extract.ts"

let n = 0;
function getTypeNode(converter: Converter, type) {
    const fname = `/getTypeNode_$${n}.ts`;
    converter.project.createSourceFile(fname, `type A = ${type};`);
    const file = converter.project.getSourceFileOrThrow(fname);
    const alias = file.getFirstDescendantByKind(SyntaxKind.TypeAliasDeclaration);
    return alias.getTypeNode();
}

describe('unit types', () => {
    it('convert string', async () => {
        const converter = new Converter();
        const typeNode = getTypeNode(converter, "string");
        const conversion = converter.typeToPython(typeNode, false);
        expect(conversion).toBe("str");
    });
    it('convert number', async () => {
        const converter = new Converter();
        const typeNode = getTypeNode(converter, "number");
        const conversion = converter.typeToPython(typeNode, false);
        expect(conversion).toBe("int | float");
    });
    it('convert union', async () => {
        const converter = new Converter();
        const typeNode = getTypeNode(converter, "string | boolean");
        const conversion = converter.typeToPython(typeNode, false);
        expect(conversion).toBe("str | bool");
    });
    describe("callable types", () => {
        it('basic', async () => {
            const converter = new Converter();
            const typeNode = getTypeNode(converter, "() => void");
            const conversion = converter.typeToPython(typeNode, false);
            expect(conversion).toBe("Callable[[], None]");
        });
        it('toplevel', async () => {
            const converter = new Converter();
            const typeNode = getTypeNode(converter, "() => void");
            const conversion = converter.typeToPython(typeNode, false, "myFunc");
            expect(conversion).toBe("def myFunc(self, /) -> None: ...");
        });
        it('optional args', async () => {
            const converter = new Converter();
            const typeNode = getTypeNode(converter, "(a?: string) => void");
            const conversion = converter.typeToPython(typeNode, false, "myFunc");
            expect(conversion).toBe("def myFunc(self, a: str | None = None, /) -> None: ...");
        });
        it('optional or null', async () => {
            const converter = new Converter();
            const typeNode = getTypeNode(converter, "(a?: string | null) => void;");
            const conversion = converter.typeToPython(typeNode, false, "myFunc");
            expect(conversion).toBe("def myFunc(self, a: str | None = None, /) -> None: ...");
        });
        it('type predicate', async () => {
            const converter = new Converter();
            const typeNode = getTypeNode(converter, "(a: any) => a is string;");
            const conversion = converter.typeToPython(typeNode, false, "myFunc");
            expect(conversion).toBe("def myFunc(self, a: Any, /) -> bool: ...");
        });
    })
});

describe.only('property signature', () => {
    it("", () => {
        const fname = "/a.ts";
        const converter = new Converter();
        converter.project.createSourceFile(fname, `
            declare var X: {f: () => void};
        `);
        const file = converter.project.getSourceFileOrThrow(fname);
        const [propsig] = file.getDescendantsOfKind(SyntaxKind.PropertySignature);
        const res = converter.convertPropertySignature(propsig);
        expect(res).toBe("def f(self, /) -> None: ...");
    });
    it("", () => {
        const fname = "/a.ts";
        const converter = new Converter();
        converter.project.createSourceFile(fname, `
            declare var X: {f?: () => void};
        `);
        const file = converter.project.getSourceFileOrThrow(fname);
        const [propsig] = file.getDescendantsOfKind(SyntaxKind.PropertySignature);
        const res = converter.convertPropertySignature(propsig);
        expect(res).toBe("f: Callable[[], None] | None");
    });
})

describe('emit', () => {
    describe('Basic conversions', () => {
        it('string type', async () => {
            const converter = new Converter();
            converter.project.createSourceFile("/a.ts", "declare var a : string;");
            const res = converter.emit([converter.project.getSourceFileOrThrow("/a.ts")]);
            expect(res.at(-1)).toBe("a: str");
        });
        it('number type', async () => {
            const converter = new Converter();
            converter.project.createSourceFile("/a.ts", "declare var a : number;");
            const res = converter.emit([converter.project.getSourceFileOrThrow("/a.ts")]);
            expect(res.at(-1)).toBe("a: int | float");
        });
        it('boolean type', async () => {
            const converter = new Converter();
            converter.project.createSourceFile("/a.ts", "declare var a : boolean;");
            const res = converter.emit([converter.project.getSourceFileOrThrow("/a.ts")]);
            expect(res.at(-1)).toBe("a: bool");
        });
    })
})

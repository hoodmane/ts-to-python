# ts-to-python

This is a project for converting typescript type definitions to Python type
stubs for use with Pyodide.

It uses the typescript compiler to generate a typescript abstract syntax tree
for the types, converts the typescript ast into an IR, then it renders this IR
into Python type hints. The most complicated step is converting the typescript
ast to the IR.

## WebIDL vs TypeScript as a source of truth

For the DOM it is also possible to generate type hints from the webidl.
[Webtypy](https://github.com/pyodide/webtypy) ingests the webidl to produce type
stubs for the Pyodide `js` module. `ts-to-python` produces the type stubs from
TypeScript `.d.ts` files. The webidl and TypeScript type declarations contain
different information. The ideal approach would combine both, but it will take
some effort to implement.

Of course many more projects have typescript type hints but no idl definitions
so the typescript approach has much broader applicability.

### What information is in the webidl but not in TypeScript declarations?

The webidls distinguish between floating point and integer types, like Python
but unlike TypeScript. Using `.d.ts` files as our source of truth, we have no
choice but to use `float | int` as the number type everywhere.

### What information is in TypeScript declarations but not in the webidl?

TypeScript declarations include types for ECMAScript builtins. They are also
available for Node and for many other packages.

### Running tests

You can run the TypeScript unit tests for the type generation with:

```sh
npm run -C type-generation test
```

To run the Python tests for the generated types, first you have to generate the
types with:

```sh
./generate_types.sh
```

Then run the tests as follows:

```sh
cd mypy-tests
uv run pytest
```

# ts-to-python

This is an experimental alternate approach to
[Webtypy](https://github.com/pyodide/webtypy).

## WebIDL vs TypeScript as a source of truth

Webtypy ingests the webidl to produce type stubs for the Pyodide `js` module.
`ts-to-python` produces the type stubs from TypeScript `.d.ts` files. The webidl
and TypeScript type declarations contain different information. The ideal
approach would combine both, but it will take some effort to implement.

### What information is in the webidl but not in TypeScript declarations?

The webidls distinguish between floating point and integer types, like Python
but unlike TypeScript. Using `.d.ts` files as our source of truth, we have no
choice but to use `float | int` as the number type everywhere.

### What information is in TypeScript declarations but not in the webidl?

TypeScript declarations include types for ECMAScript builtins. They are also
available for Node and for many other packages.

## Development

It seems to work with node >= 18.

### Generating the types

First you must install the two subdirectories:

```sh
npm install -C type-generation-input-project
npm install -C type-generation
```

then to build the types, run:

```sh
npm run -C type-generation build
```

### Running tests

You can run the unit tests for the type generation with:

```sh
npm run -C type-generation test
```

To run the Python tests for the generated types requires the following setup
first:

```sh
cd mypy-tests
python3.11 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

Then from the `mypy-tests` directory run `pytest` to run the tests.

import { PyClass, Variance, reverseVariance } from "./types";
import { PySig } from "./render";
import { TypeIR } from "./astToIR";
import { renderTypeIR } from "./extract";

export const IMPORTS = `
from collections.abc import Callable, Iterable as PyIterable, Iterator as PyIterator, MutableSequence as PyMutableSequence, Sequence as PySequence
from asyncio import Future
from typing import overload, Any, Literal, Self, TypeVar, Generic, ClassVar, Never, Protocol

from pyodide.ffi import JsProxy, JsIterable, JsIterator, JsArray, JsMutableMap, JsMap as ReadonlyMap, JsBuffer
from pyodide.webloop import PyodideFuture as PromiseLike
from _pyodide._core_docs import _JsProxyMetaClass
ConcatArray = JsArray
ArrayLike = JsArray
Dispatcher = Any
URL_ = URL
HeadersInit = PyIterable[tuple[str, str]] | Record[str, str] | Headers

# Shenanigans to convince skeptical type system to behave correctly:
#
# These classes we are declaring are actually JavaScript objects, so the class
# objects themselves need to be instances of JsProxy. So their type needs to
# subclass JsProxy. We do this with a custom metaclass.

__KT = TypeVar("__KT")  # Key type.
__VT = TypeVar("__VT")  # Value type.
__T = TypeVar("__T")


class Promise(PromiseLike[__T]):
    @classmethod
    def new(
        cls,
        executor: Callable[[], None]
        | Callable[[Callable[[__T], None]], None]
        | Callable[[Callable[[__T], None], Callable[[BaseException], None]], None],
    ) -> Promise[__T]:
        ...


class Map(JsMutableMap[__KT, __VT]):
  @classmethod
  @overload
  def new(cls) -> "JsMutableMap[__KT, __VT]":
      ...

  @classmethod
  @overload
  def new(cls, args: PySequence[tuple[__KT, __VT]]) -> "JsMutableMap[__KT, __VT]":
      ...


class _JsMeta(_JsProxyMetaClass, JsProxy):
    pass

class DoNotCallThis:
    pass

class _JsObject(metaclass=_JsMeta):
    def __new__(self, do_not_call: DoNotCallThis) -> _JsObject:
      ...

class Record(JsProxy, Generic[S, T]):
  pass
`.trim();
export const BUILTIN_NAMES = [
  "Iterable",
  "Iterator",
  "IterableIterator",
  "ArrayLike",
  "ConcatArray",
  "PromiseLike",
  "Promise",
  "Map",
  "ReadonlyMap",
  "Readonly",
  "Record",
  "Dispatcher",
  "HeadersInit",
];

const EXTRA_BASES: Record<string, string[]> = {
  Error_iface: ["Exception"],
  Uint8Array_iface: ["JsBuffer"],
  ArrayBuffer_iface: ["JsBuffer"],
  Array_iface: ["JsArray[T]"],
  Headers_iface: ["JsIterable[tuple[str, str]]"],
  HTMLCollectionBase_iface: ["JsArray[Element]"],
  NodeList_iface: ["JsArray[Node]"],
};

export function getExtraBases(name: string): string[] {
  return EXTRA_BASES[name] || [];
}

// [misc]:
//    Invariant type variable "K" used in protocol where contravariant one is expected
export const CLASS_TYPE_IGNORES = " # type:ignore[misc, unused-ignore]";
// Ignores:
// [misc]:
//    Overloaded function signature 2 will never be matched: signature 1's parameter type(s) are the same or broader
// [overload-overlap]:
//    Overloaded function signatures 1 and 6 overlap with incompatible return types
// [override]:
//    Argument 1 of "someMethod" is incompatible with supertype "superType"
//    Cannot override writeable attribute with read-only property
//    Signature of "someMethod" incompatible with supertype "superType"
export let METHOD_TYPE_IGNORES =
  " # type:ignore[misc,overload-overlap,override,unused-ignore]";
export let PROPERTY_TYPE_IGNORES = " # type:ignore[assignment,unused-ignore]";

export const TYPE_TEXT_MAP: Record<string, string> = {
  number: "int | float",
  bigint: "int",
  boolean: "bool",
  string: "str",
  void: "None",
  undefined: "None",
  null: "None",
  symbol: "Symbol",
  any: "Any",
  unknown: "Any",
  object: "Any",
  never: "Never",
  "Window & typeof globalThis": "Any",
};

export function adjustPyClass(cls: PyClass): void {
  if (cls.name === "Response") {
    // JavaScript allows static methods and instance methods to share the same
    // name, Python does not usually allow this. I think the only place where it
    // happens is with `Response.json`. We can hack it by allowing all
    // signatures on instances and on the class. This adds the missing class
    // signature.
    const lines = cls.body.split("\n");
    const idx = lines.findLastIndex((v) => v.includes("json"));
    const toAdd = [
      "@classmethod",
      "@overload",
      "def json(self, /) -> Future[Any]: ...",
    ];
    lines.splice(idx + 1, 0, ...toAdd);
    cls.body = lines.join("\n");
  }
  if (
    ["Response_iface", "Response", "String", "DataView"].includes(cls.name) ||
    (cls.name.includes("Array") && !cls.name.includes("Float"))
  ) {
    cls.body = cls.body.replaceAll("int | float", "int");
  }
}

export function adjustPySig(name: string, sig: PySig): void {
  if (["setTimeout", "setInterval"].includes(name)) {
    sig.returns = "int | JsProxy";
  }
  if (["setTimeout", "setInterval"].includes(name)) {
    sig.returns = "int | JsProxy";
  }
  if (["clearTimeout", "clearInterval"].includes(name)) {
    sig.params[0].pyType = "int | JsProxy";
  }
  if (name === "fromEntries") {
    sig.returns = "JsProxy";
  }
}

export function typeReferenceSubsitutions(
  name: string,
  typeArgs: TypeIR[],
  variance: Variance,
): string | undefined {
  if (name.endsWith("_iface")) {
    name = name.slice(0, -"_iface".length);
  }
  if (name === "URL") {
    return "URL_";
  }
  if (name === "Function") {
    return "Callable[..., Any]";
  }
  if (
    name.startsWith("Intl") ||
    ["console.ConsoleConstructor", "NodeJS.CallSite", "FlatArray"].includes(
      name,
    )
  ) {
    return "Any";
  }

  const args = () => typeArgs.map((arg) => renderTypeIR(arg, false, variance));
  const fmtArgs = () => {
    const a = args();
    if (a.length) {
      return `[${a.join(", ")}]`;
    }
    return "";
  };

  if (["Exclude", "Readonly"].includes(name)) {
    return args()[0];
  }
  if (name === "Promise") {
    return "Future" + fmtArgs();
  }
  if (name === "Iterable") {
    if (variance === Variance.contra) {
      return "PyIterable" + fmtArgs();
    } else {
      return "JsIterable" + fmtArgs();
    }
  }
  if (name === "Iterator") {
    const T = renderTypeIR(typeArgs[0], false, variance);
    const TReturn = renderTypeIR(typeArgs[1], false, variance);
    const TNext = renderTypeIR(typeArgs[2], false, reverseVariance(variance));
    const args = `[${T}, ${TNext}, ${TReturn}]`;
    if (variance === Variance.contra) {
      return `PyGenerator` + args;
    } else {
      if (TNext === "None" && TReturn === "Any") {
        // both return and next have their default value so this is a normal iterator
        return `JsIterator[${T}]`;
      }
      return "JsGenerator" + args;
    }
  }
  if (name === "IterableIterator") {
    if (variance === Variance.contra) {
      return "PyIterator" + fmtArgs();
    } else {
      return "JsIterator" + fmtArgs();
    }
  }
}

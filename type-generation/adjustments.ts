
export const IMPORTS = `
from collections.abc import Callable
from asyncio import Future
from typing import overload, Any, Literal, Self, TypeVar, Generic, ClassVar, Never, Protocol

from pyodide.ffi import JsProxy, JsIterable as Iterable, JsIterator as Iterator, JsArray, JsMutableMap as Map, JsMap as ReadonlyMap, JsBuffer
from pyodide.webloop import PyodideFuture as PromiseLike
Promise = PromiseLike
ConcatArray = JsArray
ArrayLike = JsArray
Dispatcher = Any
URL_ = URL

class Record(JsProxy, Generic[S, T]):
  pass

class IterableIterator(Iterator[T], Iterable[T], Generic[T]):
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
];

const EXTRA_BASES : Record<string, string[]> = {
    "Error_iface" : ["Exception"],
    "Uint8Array_iface": ["JsBuffer"],
    "Array_iface": ["JsArray"],
};

export function getExtraBases(name: string): string[] {
    if (name in EXTRA_BASES) {
        return EXTRA_BASES[name];
    }
    if (name.endsWith("_iface")) {
        return ["Protocol"];
    }
    return [];
}


export const CLASS_TYPE_IGNORES = "";
// Ignores:
// [misc]:
//    Overloaded function signature 2 will never be matched: signature 1's parameter type(s) are the same or broader
// [override]:
//    Argument 1 of "someMethod" is incompatible with supertype "superType"
//    Cannot override writeable attribute with read-only property
//    Signature of "someMethod" incompatible with supertype "superType"
// [overload-overlap]:
//    Overloaded function signatures 1 and 6 overlap with incompatible return types
// [unused-ignore]:
// [type-arg]:
//    Missing type parameters for generic type "?" (could be fixed by tracking type parameter defaults)
export let METHOD_TYPE_IGNORES =
  " # type:ignore[override,overload-overlap,misc,type-arg,unused-ignore]";
export let PROPERTY_TYPE_IGNORES = " # type:ignore[type-arg,unused-ignore]";

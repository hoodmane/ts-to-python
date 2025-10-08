from collections.abc import (
  Callable,
  Iterable as PyIterable,
  Iterator as PyIterator,
  AsyncIterator as PyAsyncIterator,
  AsyncIterable as PyAsyncIterable,
  Generator as PyGenerator,
  MutableSequence as PyMutableSequence,
  Sequence as PySequence,
  Awaitable,
)
from asyncio import Future
from typing import (
  overload,
  Any,
  Literal,
  Self,
  ClassVar,
  Never,
  Protocol,
)

from pyodide.ffi import (
  JsProxy,
  JsIterable,
  JsGenerator,
  JsIterator,
  JsAsyncIterator,
  JsArray,
  JsMutableMap,
  JsMap as ReadonlyMap,
  JsBuffer,
)
from pyodide.webloop import PyodideFuture
from _pyodide._core_docs import _JsProxyMetaClass

type ConcatArray[T] = JsArray[T]
type ArrayLike[T] = JsArray[T]
type ArrayLike_iface[T] = PyMutableSequence[T]
type Iterable_iface[T] = PyIterable[T]

class Thenable[T](Protocol):
  def then[TResult1, TResult2](
      self,
      onfulfilled: (Callable[[T], TResult1 | PromiseLike_iface[TResult1]])
      | None
      | None = None,
      onrejected: (Callable[[Any], TResult2 | PromiseLike_iface[TResult2]])
      | None = None,
      /,
  ) -> PromiseLike_iface[TResult1 | TResult2]:
      ...

type PromiseLike_iface[T] = Awaitable[T] | Thenable[T]


type Dispatcher = Any
type URL_ = URL
type HeadersInit = PyIterable[tuple[str, str]] | Record[str, str] | Headers

# Shenanigans to convince skeptical type system to behave correctly:
#
# These classes we are declaring are actually JavaScript objects, so the class
# objects themselves need to be instances of JsProxy. So their type needs to
# subclass JsProxy. We do this with a custom metaclass.


class Promise[T](PyodideFuture[T]):
  @classmethod
  def new(
      cls,
      executor: Callable[[], None]
      | Callable[[Callable[[T], None]], None]
      | Callable[[Callable[[T], None], Callable[[BaseException], None]], None],
  ) -> Promise[T]:
    ...


class Map[KT, VT](JsMutableMap[KT, VT]):
  @classmethod
  @overload
  def new(cls) -> "JsMutableMap[KT, VT]":
    ...

  @classmethod
  @overload
  def new(cls, args: PySequence[tuple[KT, VT]]) -> "JsMutableMap[KT, VT]":
    ...


class _JsMeta(_JsProxyMetaClass, JsProxy):
  pass


class DoNotCallThis:
  pass


class _JsObject(metaclass=_JsMeta):
  def __new__(self, do_not_call: DoNotCallThis) -> _JsObject:
    ...


class Record[S, T](JsProxy):
  def __getattr__(self, s: str) -> T:
    ...

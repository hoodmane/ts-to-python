from collections.abc import (
  Callable,
  Iterable as PyIterable,
  Iterator as PyIterator,
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
  TypeVar,
  Generic,
  ClassVar,
  Never,
  Protocol,
)

from pyodide.ffi import (
  JsProxy,
  JsIterable,
  JsIterator,
  JsArray,
  JsMutableMap,
  JsMap as ReadonlyMap,
  JsBuffer,
)
from pyodide.webloop import PyodideFuture
from _pyodide._core_docs import _JsProxyMetaClass

ConcatArray = JsArray
ArrayLike = JsArray
ArrayLike_iface = PyMutableSequence
Iterable_iface = PyIterable

__Tco = TypeVar("__Tco", covariant=True)
TResult1 = TypeVar("TResult1", covariant=True)
TResult2 = TypeVar("TResult2", covariant=True)

class Thenable(Protocol, Generic[__Tco]):
  def then(
      self,
      onfulfilled: (Callable[[__Tco], TResult1 | PromiseLike_iface[TResult1]])
      | None
      | None = None,
      onrejected: (Callable[[Any], TResult2 | PromiseLike_iface[TResult2]])
      | None = None,
      /,
  ) -> PromiseLike_iface[TResult1 | TResult2]:
      ...

PromiseLike_iface = Awaitable[__Tco] | Thenable[__Tco]


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


class Promise(PyodideFuture[__T]):
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


class Record[S, T](JsProxy):
  pass

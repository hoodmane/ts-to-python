import pytest
from pathlib import Path
from typing import Any

from io import StringIO
from collections.abc import Callable
import re


def test_type_errors(tmp_path: Path) -> None:
    from mypy import api

    source_path = "js/__init__.pyi"
    target_path = tmp_path / "js.pyi"
    with open(source_path) as f:
        removed_type_ignores = "".join(line.partition("#")[0] + "\n" for line in f)
    target_path.write_text(removed_type_ignores)
    stdout, stderr, exitcode = api.run([str(target_path)])
    try:
        assert stderr == ""
        warnings_by_code: dict[str, list[str]] = {
            k: [] for k in ["assignment", "misc", "overload-overlap", "override"]
        }
        for line in stdout.splitlines():
            if "error:" not in line:
                continue
            line = line.partition("error:")[-1]
            code = line.rpartition("[")[-1][:-1]
            message = line.rpartition("[")[0].strip()
            assert code in warnings_by_code
            warnings_by_code[code].append(message)

        pats = [
            re.compile(
                r"Overloaded function signature [0-9]+ will never be matched: signature [0-9]+'s parameter type\(s\) are the same or broader"
            ),
            re.compile(
                r'Invariant type variable "[A-Za-z]*" used in protocol where [a-zA-Z]*variant one is expected'
            ),
            re.compile(
                r'Metaclass conflict: the metaclass of a derived class must be a \(non-strict\) subclass of the metaclasses of all its bases'
            )
        ]
        for message in warnings_by_code["misc"]:
            for pat in pats:
                if pat.fullmatch(message):
                    break
            else:
                raise Exception("Unexpected error message:\n" + message)
    except Exception:
        Path("issues.txt").write_text(stdout)
        raise


@pytest.mark.mypy_testing
def type_exception(a: bool) -> None:
    from js import Error, DOMException

    if a:
        raise DOMException.new("oops")
    else:
        raise Error.new("hi")


@pytest.mark.mypy_testing
def type_xmlhttprequest(url: str) -> StringIO:
    from js import XMLHttpRequest

    req = XMLHttpRequest.new()
    req.open("GET", url, False)
    req.send()
    # this were a bit more specific
    reveal_type(req.response)  # R: Any
    return StringIO(req.response)


@pytest.mark.mypy_testing
def type_fetch() -> None:
    from js import fetch


@pytest.mark.mypy_testing
def type_eval() -> None:
    from js import eval

    reveal_type(eval("abc"))  # R: Any


@pytest.mark.xfail("setTimeout has wrong type")
@pytest.mark.mypy_testing
def type_timeout(callback: Callable[[], None], timeout: int) -> None:
    from js import setTimeout, clearTimeout, setInterval, clearInterval
    from pyodide.ffi import JsProxy

    timeout_retval: int | JsProxy = setTimeout(callable, timeout)
    clearTimeout(timeout_retval)

    setInterval(callable, timeout)
    clearInterval(timeout_retval)


@pytest.mark.mypy_testing
def type_object() -> None:
    from js import Object
    from pyodide.ffi import JsProxy

    a: JsProxy = Object.fromEntries([(1, 2)])


@pytest.mark.mypy_testing
def type_buffer() -> None:
    from js import Uint8Array, ArrayBuffer

    a = Uint8Array.new(range(10))
    assert ArrayBuffer.isView(a)

    from tempfile import TemporaryFile

    with TemporaryFile() as f:
        a.to_file(f)
        f.seek(0)
        assert f.read() == a.to_bytes()

    import js

    assert js.Float64Array.BYTES_PER_ELEMENT == 8


@pytest.mark.mypy_testing
def type_json() -> None:
    from js import JSON, Array
    from pyodide.ffi import to_js
    import json

    class Pair:
        __slots__ = ("first", "second")

        def __init__(self, first: int | Pair, second: int):
            self.first = first
            self.second = second

    p1 = Pair(1, 2)
    p2 = Pair(1, 2)
    p2.first = p2

    def default_converter(value: Any, convert: Any, cacheConversion: Any) -> Any:
        result = Array.new()
        cacheConversion(value, result)
        result.push(convert(value.first))
        result.push(convert(value.second))
        return result

    p1js = to_js(p1, default_converter=default_converter)
    p2js = to_js(p2, default_converter=default_converter)

    assert json.loads(JSON.stringify(p1js)) == [1, 2]


@pytest.mark.mypy_testing
def type_document() -> None:
    from js import document

    el = document.createElement("div")
    assert el.tagName == "DIV"
    assert bool(el)
    document.body.appendChild(el)
    assert document.body.children
    assert len(document.body.children) == 1
    assert document.body.children[0] == el
    assert repr(document) == "[object HTMLDocument]"
    assert len(dir(el)) >= 200
    assert "appendChild" in dir(el)


@pytest.mark.mypy_testing
def type_canvas() -> None:
    from js import document

    canvas = document.createElement("canvas")
    canvas.id = "canvas"
    canvas.width = 320
    canvas.height = 240

    canvas.style.position = "fixed"
    canvas.style.bottom = "10px"
    canvas.style.right = "10px"

    gl = canvas.getContext(
        "webgl2",
        powerPreference="high-performance",
        premultipliedAlpha=False,
        antialias=False,
        alpha=False,
        depth=False,
        stencil=False,
    )
    document.body.appendChild(canvas)


@pytest.mark.xfail("HTMLElement has no length")
@pytest.mark.mypy_testing
def type_document() -> None:
    from js import document

    assert document.body


@pytest.mark.mypy_testing
def type_headers() -> None:
    from js import Headers
    from typing import Iterable

    a: Iterable[tuple[str, str]] = []
    Headers.new(a)


@pytest.mark.mypy_testing
def type_request() -> None:
    from js import Request, Headers, AbortController

    path = "a"
    method = "GET"
    sig = AbortController.new()
    Request.new(
        path,
        method=method,
        headers=Headers.new(),
        body="abc",
        signal=sig.signal,
    )


@pytest.mark.mypy_testing
def type_iterable() -> None:
    from js import Int8Array

    for x in Int8Array.new([1, 2, 3]):
        reveal_type(x)  # R: Union[builtins.int, builtins.float]


@pytest.mark.mypy_testing
def type_sized() -> None:
    from js import Int8Array, Set

    l = Int8Array.new([1, 2, 3])
    reveal_type(len(l))  # R: builtins.int
    s = Set.new(l)
    reveal_type(len(s))  # R: builtins.int


@pytest.mark.xfail("Map issue?")
@pytest.mark.mypy_testing
def type_map() -> None:
    from js import Map

    len(Map.new([(1, 2)]))


@pytest.mark.mypy_testing
def type_weakmap() -> None:
    from js import WeakMap

    a = WeakMap.new([(1, "a")])
    1 in a
    a[1]
    a[1] = "d"
    del a[1]
    # fmt: off
    "1" in a  # E: Unsupported operand types for in ("str" and "WeakMap[int, str]")
    a["1"]  # E: Invalid index type "str" for "WeakMap[int, str]"; expected type "int"
    del a["1"]  # E: Argument 1 to "__delitem__" of "WeakMap_iface" has incompatible type "str"; expected "int"
    a[1] = 1  # E: Incompatible types in assignment (expression has type "int", target has type "str")
    a["1"] = "1"  # E: Invalid index type "str" for "WeakMap[int, str]"; expected type "int"
    # fmt: on


@pytest.mark.mypy_testing
def type_contains() -> None:
    from js import WeakMap, Set, Headers

    map = WeakMap.new([(1, 3)])
    assert 1 in map
    s = Set.new([1, 2, 3])
    assert 1 in s
    assert "SomeHeader" not in Headers.new()

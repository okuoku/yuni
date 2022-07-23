SIBR0012: Zero length read can return eof-object
================================================

Disallow: `STklos`

Early-cut: `Cyclone`

EOF: `Gambit`

Nil: (otherwise)

Some Scheme do not return "" (null-string) for zero-length `read-string` procedure.

```scheme
(read-string 0 (open-input-string "xxxx")) ;; => "" / #!eof / ERROR
```

On Gambit, it will always return `#!eof` thus a textual port can return
valid datum after `#!eof` if subsequent reads had non-zero length.

Notes
=====

Cyclone will prefer `#!eof` over null string if the port reached to the end(Early-cut).

STklos do not allow zero-length reads from buffered port(Disallow).


Reference
=========

* https://github.com/okuoku/yuni/issues/132

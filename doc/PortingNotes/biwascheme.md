BiwaScheme support (prototype)
==============================

* [GitHub](https://github.com/biwascheme/biwascheme)
* [Official website](https://www.biwascheme.org/)
* [Reference](https://www.biwascheme.org/doc/reference.html)

BiwaScheme is a Scheme interpreter implemented in JavaScript with R6RS subset
libraries without `syntax-case` support (at least for now).

Yuni's BiwaScheme support implemented using [Generic][] runtime.

Limitation
----------

See [Generic][] for generic runtime limitations.

JavaScript strings are immutable. As BiwaScheme uses JavaScript string
directly, procedures for immutable strings such as `string-set!` or
`string-copy!` are not provided.

BiwaScheme's `define-macro` does not have any scoping; it must be a global
definition. Although Yuni still provide (limited -- as described in [Generic][]
) `define-syntax` and `syntax-rules`, there is no support for:

* `define-syntax` inside of any lexical scope (ie. inside of `let` or `lambda`)
* `let-syntax` or `letrec-syntax`

biwasyuni
---------

Although BiwaScheme provides its own interpreter `biwas` to run on Node.js, 
Yuni requires some extension to the interpreter runtime.

[biwasyuni][] provides several library extensions that needed to run Yuni's 
R6RS-lite format programs.

* R7RS extended version of `string->list` etc
* R7RS bytevectors
* Async-aware `load` procedure
* Blocking File I/O on R7RS ports on Node.js fs library


[Generic]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/Generic.md
[biwasyuni]: https://github.com/okuoku/biwasyuni

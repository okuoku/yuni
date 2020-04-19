yuni Language and Library Limitations
=====================================

Although yuni mostly supports R7RS features, there are some intentional 
omissions and limitations.

Language features
-----------------

### `call/cc`

Omits: `call-with-current-continuation` `call/cc`

Some Scheme implementations such as Kawa do not support full-continuation
by default. 

### Tail calls

### Eval

Omits: `environment` `scheme-report-environment` `null-environment` `interaction-environment` `eval`

Library
-------

### Library body

Yuni does not have guaranteed semantics on library-body's sideeffect. 

Library body is treated as it was surrounded with `let` ; it is an error that
forms appeared between top-level definitions.

Library body might be invoked multiple times when imported. 

### Naming and lookup

Omits: `define-library`

Limits: `library`

Unlike R6RS, yuni library name cannot have version specifier.

Unlike R7RS, yuni library name must be symbols, numbers are not allowed.
Since yuni uses R6RS library format, it does not provide `define-libray` form
at all.

### Symbol rename and excluding

Limits: `only` `except` `rename`

Although yuni still supports export/import of syntax symbols, they cannot be
processed with `only` `rename` and `except` since yuni does not require strict
scoping for macros.

### File-level inclusion

Omits: `include` `include-ci`

R7RS does not provide portable semantics for include path handling. 

Syntax
------

### Macro limitations

Limits: `define-syntax`

Omits: `let-syntax` `letrec-syntax`

Yuni requires various limitations on macro definition. 

1. `syntax-rules` is only hygiene when yuni-specific syntax were used
2. `define-syntax` must be placed on top-level
3. Macro name must be determined prior expansion; macro cannot expand into `define-syntax` or `syntax-rules`
4. `SIBR0008` : `syntax-rules` must on be immediately after `define-syntax`

### `cond-expand`

Omits: `cond-expand` `features`

Currently, Yuni omits any `cond-expand` related features because it lack any
established standard. 

Standard procedures
-------------------

### Unicode

Limits: `utf8->string` `string->utf8`

Although these procedures have `utf8` name to keep compatibility, it may
not support actual UTF-8.

### Unequal length argument on `map` and `for-each`

Limits: `map` `for-each` `vector-map` `vector-for-each` `string-map` `string-for-each`

Yuni takes R6RS semantics; it is an error that specifing different lengths of 
collection.

Data types
----------

### Mutable strings

Omits: `string-set!` `string-copy!` `string-fill!`

Since some of other languages do not have mutable strings, yuni omits 
immutable strings from its library.

(Unlike R6RS, pairs are mutable.)

### Bignums

### Complex / ratnums

### Bytevectors

Limits: `string?` `bytevector?`

Some Scheme implementations like SCM uses string-as-bytevector strategy that
means strings are interchangeable with bytevectors. 

On such implementation an object can return `#t` for both procedures.

I/O
---

### Binary ports

Limits: `textual-port?` `binary-port?`

Some Scheme implementations like SCM uses string-as-bytevector strategy that
means strings are interchangeable with bytevectors. 

On such implementation a port can return `#t` for both procedures.

### Port state query

Omits: `input-port-open?` `output-port-open?`

Yuni do not support this behaviour since it was not required on R6RS or earlier.

In addition, implementations can keep standard ports open even if `close-port`
called on standard ports.

### Non-blocking operations

Omits: `char-ready?` `u8-ready?` 

Yuni omits non-blocking operations from the library because it is not
universally available nor implemented reliably.

### Parameterized default I/O ports

Omits: `with-input-from-file` `with-output-to-file`

Limits: `current-input-port` `current-output-port` `current-error-port`

R7RS requires current-ports are parameter object. Yuni does not support
this behaviour since it was not required on R6RS or earlier.


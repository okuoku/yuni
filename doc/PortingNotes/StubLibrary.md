Stub library
============

Since Yuni uses R6RS-lite format; R6RS library format with R7RS small libraries, most Scheme implementations require a conversion step to convert Yuni's R6RS-lite libraries into their native format.

| impl       |std | File extension | Filter              | Remarks                      |
|:-----------|:--:|:--------------:|:--------------------|:-----------------------------|
|gauche      |R7RS|.scm            |Strip-Keywords       |[SIBR0005][]                  |
|chibi-scheme|R7RS|.sld            |Include-Basename     |Refered as `chibi` for now    |
|picrin      |R7RS|.scm            |Reverse-import-export|No library search-load feature|
|chicken     |R7RS|.scm            |Strip-Stdaux         |No library search-load feature|
|kawa        |R7RS|.sld            |none                 |Always use absolute paths     |
|sagittarius |R7RS|.sagittarius.sls|Strip-Keywords       |R6RS Hybrid                   |
|racket      |R6RS|.mzscheme.sls   |none                 |Require `#!r6rs`              |
|guile       |R6RS|.guile.sls      |Strip-Stdaux         |Special file extension        |
|larceny     |-   |.sls            |none                 |                              |
|ironscheme  |-   |.sls            |none                 |                              |
|chez        |-   |.sls            |none                 |                              |
|vicare      |-   |.sls            |none                 |                              |
|nmosh       |-   |.sls            |none                 |                              |
|gambit      |-   |.sls            |none                 |yunifake R5RS loader          |
|mit-scheme  |-   |.sls            |none                 |yunifake R5RS loader          |

`SIBR` refers Scheme Imprementation Behaviour Registry(SIBR) number. See `doc/sibr`.

R6RS implementations such as Larceny or Chez Scheme do not require any stub library and they will read Yuni libraries directly without stub libraries i.e. just adding a library path should work. Sagittarius is an exception on this; it can read R6RS library but it requires stub libraries because it still require export filtering.

CMake bootstrap will generate stub libraries for Yuni itself under `yunified/runtime` directory.

Export filtering
----------------

Currently, we have 2 kinds of export filtering.

`Strip-Stdaux`: Because of [SIBR0001][], we have to filter-out Scheme standard's aux-keywords such as `unquote` or `=>` because these are unbound on SIBR0001 implementations. The filter just removes these from export list to keep them unbound.

`Strip-Keywords`: Some implementation reads some symbol-like notation as "Keywords" [SIBR0009][].

Include path mapping (R7RS)
---------------------------

Include path mapping is probably the most untrivial part of stub libraries. 

In general, stub library will `include` original R6RS-lite library so we can keep line numbers etc. Unfortunately, `include` path handling is varies between implementations.

`gauche`: It seems it cannot traverse-up beyond R7RS library description. Yuni will copy R6RS-lite library side-by-side.

`chibi-scheme`: `include` also refers specified library-path. Yuni will add R6RS-lite libraries to the library-path so it can lookup them by `include`.

`kawa`: Due to path handling behaviour on Win32, Yuni will always use an absolute path as `include` path.

Other implementations will use relative path from stub library.


Include path handling (R6RS)
----------------------------

Unlike other R6RSs, `racket` and `guile` require export filtering so they will need stub libraries too.

R6RS standard lacks `include` feature on its library format so Yuni will use implementation specific `include` syntax.

Special library format
----------------------

Picrin implements `include` as library and it seems the implementation only accepts `export` => `import` order which is reverse of R6RS library format [SIBR0006][].

Racket requires `#!r6rs`(= `#lang r6rs`) on the file header for every R6RS libraries.



[SIBR0001]: https://github.com/okuoku/yuni/blob/master/doc/sibr/SIBR0001-AUX-KEYWORDS.md
[SIBR0005]: https://github.com/okuoku/yuni/blob/master/doc/sibr/SIBR0005-C-STYLE-INCLUDEPATH.md
[SIBR0006]: https://github.com/okuoku/yuni/blob/master/doc/sibr/SIBR0006-EXPORT-AS-LIBRARY.md
[SIBR0009]: https://github.com/okuoku/yuni/blob/master/doc/sibr/SIBR0009-DEFAULT-KEYWORD-SYNTAX.md

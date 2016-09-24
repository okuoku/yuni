Application launch strategies
=============================

[StubLibrary][] generation is also required when launching user app which uses Yuni libraries. 

| impl       |Prepare |Inc |Launch     | Remarks                      |
|:-----------|:------:|:--:|:---------:|:-----------------------------|
|gauche      |Copyfile|SxS |-          |                              |
|chibi-scheme|Stubfile|Lib |-          |                              |
|picrin      |?       |?   |yuniloader |                              |
|chicken     |?       |?   |yuniloader |                              |
|kawa        |?       |n/a |yuniloader |                              |
|sagittarius |Stubfile|Abs |AutoCache  |                              |
|racket      |Stubfile|Abs |AutoCompile|                              |
|guile       |Stubfile|Abs |AutoCache  |                              |
|larceny     |Addpath |-   |-          |                              |
|ironscheme  |Addpath |-   |yuniloader |                              |
|chez        |Addpath |-   |-          |                              |
|vicare      |Addpath |-   |-          |                              |
|nmosh       |Addpath |-   |AutoCache  |                              |
|gambit      |?       |-   |yuniloader |                              |
|mit-scheme  |?       |-   |yuniloader |                              |

App launch process
==================

App launch procedure can be divided into 3 phases. 

* `Generate`: Generate phase will generate stub libraries. `Addpath` implementations in above table do not require this phase because they do not need stub library entirely.
* `Compile`: Compile phase will compile app into native code or bytecode.
* `Execute`: Load and execute the app.

`Generation` or `Compile` phases may or may not be implemented because some implementations do not require them. 

Generate
--------

Generate phase has 3 storategies and some variants for `include` path handling.

`Addpath` does nothing. These implementations do not require stub library at all.

`Copyfile` copies R6RS-lite formatted library into private directory then generate stub libraries just as `Stubfile`. This one is Gauche specific workaround.

`Stubfile` generates stub library. This strategy(and `Copyfile`) has 3 variants for `include` path notation:

 * `SxS`(Side-by-side): Copyfile strategy copies original library side-by-side with stub library. Include path will be basename of the original library.
 * `Lib`(library path): Add library path just same as `Addpath` and include path will be relative path of the original library, *relative to original library path*. This one is Chibi-scheme specific workaround because they do not accept absolute paths on `include` syntax.
 * `Abs`(absolute): Include path will be just a absolute path of the original library.



[StubLibrary]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/StubLibrary.md

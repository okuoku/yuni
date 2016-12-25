Application launch strategies
=============================

[StubLibrary][] generation is also required when launching user app which uses Yuni libraries. 

| impl       |Prepare |Inc |Gen |Comp| Remarks                      |
|:-----------|:------:|:--:|:--:|:--:|:-----------------------------|
|gauche      |Stubfile|Abs |X   |    |                              |
|chibi-scheme|Stubfile|Lib |X   |    |                              |
|picrin      |?       |?   |X   |    |yuniloader                    |
|chicken     |?       |?   |X   |    |yuniloader                    |
|kawa        |?       |n/a |X   |    |yuniloader                    |
|sagittarius |Stubfile|Abs |X   |    |AutoCache                     |
|racket      |Stubfile|Abs |X   |X   |                              |
|guile       |Stubfile|Abs |X   |    |AutoCache                     |
|larceny     |Addpath |-   |    |X   |                              |
|ironscheme  |Addpath |-   |    |    |yuniloader                    |
|chez        |Addpath |-   |    |    |                              |
|vicare      |Addpath |-   |    |    |                              |
|nmosh       |Addpath |-   |    |    |AutoCache                     |
|gambit      |?       |-   |    |X   |yuniloader                    |
|mit-scheme  |?       |-   |    |X   |yuniloader                    |

App launch process
==================

App launch procedure can be divided into 3 phases. 

* `Generate`: Generate phase will generate stub libraries. `Addpath` implementations in above table do not require this phase because they do not need stub library entirely.
* `Compile`: Compile phase will compile app into native code or bytecode.
* `Execute`: Load and execute the app. (Just generate shell script that follows [CommandLine] spec for each impl.s)

`Generation` or `Compile` phases may or may not be implemented because some implementations do not require them. 

Generate
--------

Generate phase has 2 storategies and some variants for `include` path handling.

`Addpath` does nothing. These implementations do not require stub library at all.

`Stubfile` generates stub library. This strategy has 2 variants for `include` path notation:

 * `Lib`(library path): Add library path just same as `Addpath` and include path will be relative path of the original library, *relative to original library path*. This one is Chibi-scheme specific workaround because they do not accept absolute paths on `include` syntax.
 * `Abs`(absolute): Include path will be just a absolute path of the original library.



[StubLibrary]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/StubLibrary.md
[CommandLine]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/CommandLine.md

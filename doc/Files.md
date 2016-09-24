FILES
=====

Directories
-----------

Yuni libraries are splitted into several directories.

* (Part of distribution)
 * `lib`         - Main library directory.
 * `lib-compat`  - Implementation specific libraries.
 * `lib-r6rs`    - R7RS base libraries for R6RS. Renamed as `(scheme *)`.
 * `lib-runtime` - Implementation specific format libraries.

Every library directories except `lib-runtime` should be added to `*library-directories*` variable of `config/config.scm`. `lib-runtime` will not be parsed during bootstrapping.

Syntax(R6RS-lite format)
------------------------

Libraries in `lib` and `lib-compat` have to be written in R6RS-lite format; intersection of R6RS and R7RS lexical syntax. That mean you have to avoid:

* use of #vu8() or #u8(). These are not compatible between R6RS / R7RS.
* |(vertical bar) for symbol. Ditto.
* Square brackets `[` and `]`. These are not allowed in R7RS.

R6RS-lite is designed to be:

* loaded directly on R6RS implementations such as NMosh 
* parsed directly on R6RS and R7RS implementation's native reader. ie. no external reader will be required.

Every R7RS implementations and some R6RS cannot load R6RS-lite libraries directly. Yuni generates "stub library" for these implementations. See [PortingNotes/StubLibrary.md][] for details.


[PortingNotes/Stublibrary.md]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/StubLibrary.md

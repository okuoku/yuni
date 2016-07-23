yuni
----

R7RS/R6RS Scheme portability library

[![Build Status](https://travis-ci.org/okuoku/yuni.svg?branch=master)](https://travis-ci.org/okuoku/yuni)
[![Build status](https://ci.appveyor.com/api/projects/status/0mtb3ldlwk2qwvck/branch/master?svg=true)](https://ci.appveyor.com/project/okuoku/yuni/branch/master)

`yuni` is a collection of R6RS/R7RS compatible libraries. It's under development; still USELESS for most people.

## Libraries

 * `(yuni scheme)` - R7RS base library, also available on R6RS. See also: [r7b-Issues][]
 * `(yuni core)` - Basic structure and typing
 * `(yuni ffi *)` - Static binding FFI (under construction)

## Implementations

See also: [PortingNotes][] and [Blocker-Issues][]

Implementations with FFI compatibility layer:

 * [nmosh](https://github.com/okuoku/mosh)
 * [chibi-scheme](http://synthcode.com/wiki/chibi-scheme)
 * [Racket](https://racket-lang.org/) with `srfi-lib` and `r6rs-lib` packages
 * [Sagittarius](https://bitbucket.org/ktakashi/sagittarius-scheme/wiki/Home)
 * [Gauche](http://practical-scheme.net/gauche/) 0.9.4 or later
 * [Guile](http://www.gnu.org/software/guile/) 2.0 or later
 * [Vicare](http://marcomaggi.github.io/vicare.html)
 * [Chicken](http://www.call-cc.org/) interpreter with `r7rs` egg

Bootstrapped, but no FFI yet:

 * [IronScheme](http://ironscheme.codeplex.com/)
 * [Kawa](http://www.gnu.org/software/kawa/)
 * [ChezScheme](https://github.com/cisco/ChezScheme)
 * [Gambit](http://gambitscheme.org/) with Rapid-scheme frontend
 * [Larceny](http://larcenists.org/) - Lacks non-movable bytevectors #46

## Licensing

(TBD. Each source should include its own license terms. We will provide combined license document later for binary distributions.)

Build
-----

TBD: Document CMake buildsystem here.

Every implementation requires `lib-stub` which will contain library import stubs.
To generate library import stub, install nmosh from http://storage.osdev.info/pub/mosh/mosh-current.tar.gz and run:

 `run/buildstub.sh`

[Blocker-Issues]: https://github.com/okuoku/yuni/issues?q=is%3Aissue+is%3Aopen+label%3AExtern-Blocker
[r7b-Issues]: https://github.com/okuoku/yuni/issues?q=is%3Aissue+is%3Aopen+label%3ALib-R7RSBridge
[PortingNotes]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes.markdown

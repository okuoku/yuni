yuni
----

R7RS/R6RS/Generic Scheme portability platform

`yuni` is a portability layer for Scheme applications that would allow to run single program/library on various Scheme implementations (with some limitations). It's under development; still USELESS for most people.

## Libraries

 * `(yuni scheme)` - Subset of R7RS base library, also available on R6RS.
 * `(yuni core)` - Basic structure and typing
 * `(yuni ffi *)` - Static binding FFI (under construction)

## Language Features and Limitations

In short, yuni provides subset of R6RS library loader and subset of R7RS base libraries. On the other hand, Yuni does not try to fill implementation limitations.

See also: [Basic library sample](https://github.com/okuoku/yuni/tree/master/samples/hellolib)

### Features

- Common: Minimalistic common FFI implementation (TBD)
- For R6RS: R7RS base libraries including SRFI-46 `syntax-rules` 
- For R7RS: R6RS-lite library loader that can convert R6RS `library` form into `load` or `define-library` form (if supported)
- For Generic Scheme with `define-macro`: R7RS base libraries and non-hygienic `syntax-rules` and `library` implementation

### Limitations

Yuni is NOT designed for writing any benchmarks. Library wrapper can kill some form of optimizations which can be critical for performance.

See also: [SIBR](); Yuni's collection of Scheme implementation behaviour reports

- FFI implementation is incomplete
- No `rename` on `library` form, especially Generic Scheme does not support renaming syntax such as `let` or `define`
- Some Scheme might not have R6RS/R7RS features; such as full numeric-tower, tail-calls, `eval`, etc

## Getting Started

(Not ready yet.)

## Implementations

See also: [PortingNotes][]

Implementations with FFI compatibility layer:

 * [chibi-scheme](http://synthcode.com/wiki/chibi-scheme)
 * [Racket](https://racket-lang.org/) with `srfi-lib` and `r6rs-lib` packages
 * [Sagittarius](https://bitbucket.org/ktakashi/sagittarius-scheme/wiki/Home)
 * [Gauche](http://practical-scheme.net/gauche/) 0.9.5 or later
 * [Guile](http://www.gnu.org/software/guile/) 2.0 or later
 * [Chicken](http://www.call-cc.org/) interpreter with `r7rs` egg
 * [Gambit](http://gambitscheme.org/) with experimental R5RS support(BSD3/GPL2+)
 * [ChezScheme](https://github.com/cisco/ChezScheme)

Bootstrapped, but no FFI yet:

 * [IronScheme](https://github.com/leppie/IronScheme)
 * [Kawa](http://www.gnu.org/software/kawa/) 2.2 or later
 * [MIT/GNU Scheme](https://www.gnu.org/software/mit-scheme/) with experimental R5RS support(BSD3/GPL2+)
 * [BiwaScheme](https://github.com/biwascheme/biwascheme) (experimental)
 * [s7](https://ccrma.stanford.edu/software/snd/snd/s7.html) with experimental Generic Scheme support(BSD3)

(Support was temporally removed for now):

 * [nmosh](https://github.com/okuoku/mosh)
 * [Vicare](http://marcomaggi.github.io/vicare.html)
 * [Larceny](http://larcenists.org/)
 * [Picrin](https://github.com/picrin-scheme/picrin) with yuniffi patch

## License

Public domain (CC0-1.0). Yuni R6RS/R7RS runtime component is released into public domain by the author. See `COPYING.CC0` for full license text.

Yuni R5RS support uses Alexpander(BSD3/GPL2+).

Yuni generic scheme support includes `syntax-rules` implementation from Chibi-scheme.

NOTE: Following directories contain copyrighted materials from other projects.

 * `apidata`
 * `external`
 * `integration`
 * `tests`

These directories are not part of Yuni runtime library.


[SIBR]: https://github.com/okuoku/yuni/tree/master/doc/sibr
[PortingNotes]: https://github.com/okuoku/yuni/tree/master/doc/PortingNotes

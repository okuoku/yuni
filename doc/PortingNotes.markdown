Porting Notes
-------------

In Yuni, every libraries written in:

* `.sls` extension
* R6RS style library definition(R6RS-light)

Obviously, we need some adaptation layer to support various Scheme implementatons. This file describes how Yuni libraries adopt those implementations.

SIBR(Scheme Implementation Behaviour Report)
============================================

Yuni has repository for scheme implementation behaviour and its workaround implemented in Yuni.

See `doc/sibr` for list.

FILES
=====

Directories
-----------

Yuni libraries are splitted into several directories.

* (Part of distribution)
 * `lib` - R6RS-lite. Main library directory.
 * `lib-compat` - R6RS-lite. Implementation specific libraries.
 * `lib-r6rs` - R6RS. R7RS base libraries for R6RS. Renamed as `(scheme *)`.
 * `lib-runtime` - Implementation specific format libraries. 
* (Built with bootstrapper)
 * `lib-stub` - Implementation specific format. 

Every library directories except `lib-runtime` should be added to `*library-directories*` variable of `config/config.scm`. `lib-runtime` will be ignored from the build system.

Syntax(R6RS-lite format)
------------------------

Libraries in `lib` and `lib-compat` have to be written in R6RS-lite format; intersection of R6RS and R7RS lexical syntax. That mean you have to avoid:

* use of #vu8() or #u8(). These are not compatible between R6RS / R7RS.
* |(vertical bar) for symbol. Ditto.
* Square brackets `[` and `]`. These are not allowed in R7RS.

R6RS-lite is designed to be:

* loaded directly on R6RS/R7RS hybrid implementations such as NMosh or Sagittarius.
* parsed directly on R6RS and R7RS implementation's native reader. ie. no external reader will be required.

Library adaptation and renaming
-------------------------------

Every libraries except under `lib` will be renamed before its use to provide consistent library namings. Its renaming rules are described at `config/config.scm`.

Library renaming is done by auto-generated stub libraries depending on target implementation. Stub libraries need to be regenerated when base R6RS-lite library changed its import or export set.

## R6RS except racket

`lib-stub/<impl>` directory will have stub-library:

```scheme
(library (r7b-util eval) 
  (export scheme-report-environment null-environment 
          eval environment load interaction-environment) 
  (import (nmosh-r7b-util eval)))
```

to rename R6RS-lite library `(<impl>-r7b-util eval)` into `(r7b-util eval)`.

## R7RS

Since R7RS uses `define-library` syntax instead of `library` which can be found in R6RS libraries (and yuni which uses R6RS-lite format), 

```scheme
(define-library (yuni util invalid-form)
  (export define-invalid-forms define-invalid-form)
  (import (scheme base) (yuni-runtime r7rs))
  (include "lib/yuni/util/invalid-form.sls"))
```

Injected library `(yuni-runtime r7rs)` has `library` macro which expands into library body excluding `export` or `import` sections.


SCHEME
======

Scheme implementation dependent notes.

R6RS/R7RS Hybrid
----------------

Since Yuni uses R7RS-small as Scheme base library, R6RS/R7RS hybrid implementations will be supported as first-class.

## NMosh

Yuni uses Nmosh as its own reference implementation. NMosh uses yuni as part of its R7RS compatibility layer.

## Sagittarius

Yuni mostly uses R7RS side of Sagittarius.

Sagittarius' keyword syntax conflicts some of yuni library. SIBR0009.

R6RS
----

Yuni includes R7RS-small library implementation for R6RS (r7b, r7rs-bridge). Following pure-R6RS implementation will be supported through with it.

Several R6RS implements meta-level for library imports. So we need a proxy library to equalise meta-levels.

* (r6rs-common-yuni compat macro primitives)
 * Low-level macro primitives

Yuni's R7RS compatibility is not perfect on some aspects. For example, it does not allow `_` as literal in `syntax-rules` unlike R7RS. SIBR0003.

## Racket

Racket requires `#!r6rs` for each R6RS styled library so we have to generate import stub for each libraries. See R7RS section below.

For "Minimal" installation of Racket, Yuni will require `r6rs-lib` and `srfi-lib` packages. 

* SRFI-0 is not implemented at all on (racket-srfi i0)
* Some library names of R7RS such as `(scheme base)` are not implemented because of name conflict.

## Guile

* SRFI-0 is not implemented at all on (guile-srfi i0)

## IronScheme

Yuniffi is not supported yet.

R7RS
----

## Chibi-scheme

Yuniffi supported through C extension.

## Gauche

Yuniffi supported through C extension.

## Chicken

Yuniffi supported through embedded C code which required to be pre-compiled.

## Kawa

Yuniffi is not supported yet.

## picrin

* Not yet. See [upstream bug #345](https://github.com/picrin-scheme/picrin/issues/345).

Picrin has no native FFI. Yuni has a module to support to enable yuniffi on it.

## Foment

* No FFI/extension modules yet.

Others
------

Yuni also have some support for pre-R6RS/R7RS Scheme implementations. 

Basic requirements are:

- `SRFI-6` Basic string ports
- `SRFI-30` Nested multi-line comments
- `SRFI-46` (Optional) Basic syntax-rules extensions - can use Alexpander 

## Gambit

Currently, Yuni will use bundled Alexpander to expand the library. 

Alternatively, Yuni also supports [Rapid-gambit](https://github.com/okuoku/rapid-gambit) as R7RS compatibility layer/expander. Rapid-gambit expander is port of [Rapid-scheme](https://www.rapid-scheme.org).

## MIT/GNU Scheme

The implementation lacks `SRFI-46`. Yuni will use Alexpander.

Bytevectors are implemented with strings.

Yuniffi is not ported yet.

Porting Notes
-------------

In Yuni, every library written in:

* `.sls` extension
* R6RS style library definition(R6RS-light)

Obviously, we need some adaptation layer to support various Scheme implementatons. This file describes how Yuni libraries adopt those implementations.

See ../HACKING.markdown for generic library usage and standard commandline to develop the library itself.

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
* (Built with run/buildstub.sh)
 * `lib-stub` - Implementation specific format. 

`run/*.sh` describes which directories have to be used with each implementations.

Every library directories except `lib-runtime` should be added to `*library-directories*` variable of `config/config.scm`. `lib-runtime` will be ignored from the build system.

Syntax
------

Libraries in `lib` and `lib-compat` have to be written in R6RS-lite format; intersection of R6RS and R7RS lexical syntax. That mean you have to avoid;

* use of #vu8() or #u8(). These are not compatible between R6RS / R7RS.
* |(vertical bar) for symbol. Ditto.
* Square brackets `[` and `]`. These are not allowed in R7RS.

R6RS-lite is designed to be loaded directly on R6RS implementations such as NMosh or Sagittarius. 

Renaming
--------

Every libraries except under `lib` will be renamed before its use to provide consistent library namings. 

Library renaming rules are described at `config/config.scm`.


SCHEME
======

Scheme implementation dependent notes.

R6RS/R7RS Hybrid
----------------

Since Yuni uses R7RS-small as Scheme base library, R6RS/R7RS hybrid implementations will be supported as first-class.

## NMosh

Yuni uses Nmosh as its own reference implementation.

## Sagittarius

R6RS
----

Yuni includes R7RS-small library implementation for R6RS (r7b, r7rs-bridge). Following pure-R6RS implementation will be supported through with it.

Several R6RS implements meta-level for library imports. So we need a proxy library to equalise meta-levels.

* (r6rs-common-yuni compat macro primitives)
 * Low-level macro primitives

## Racket

Racket requires `#!r6rs` for each R6RS styled library so we have to generate import stub for each libraries. See R7RS section below.

For "Minimal" installation of Racket, Yuni will require `r6rs-lib` and `srfi-lib` packages. 

* SRFI-0 is not implemented at all on (racket-srfi i0)

## Guile

* SRFI-0 is not implemented at all on (guile-srfi i0)

## IronScheme

R7RS
----

R7RS uses `define-library` form which is different and extended from R6RS' `library` form. To support R7RS implementations, "import stub"s will be generated under `lib-stub` directory.

## Chibi-scheme

* Supported.

## Gauche

* Supported.

## Chicken

* Supported.

## picrin

* Not yet. There is no way to specify library path.

## Foment

* No un-hygienic macro yet.

Others
------

## Gambit


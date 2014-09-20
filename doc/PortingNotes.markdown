Porting Notes
-------------

In Yuni, every library written in:

* `.sls` extension
* R6RS style library definition(R6RS-light)

Obviously, we need some adaptation layer to support various Scheme implementatons. This file describes how do Yuni libraries adopts those implementations.

See ../HACKING.markdown for generic library usage and standard commandline to develop the library itself.

FILES
=====

Directories
-----------

Yuni libraries are splitted into several directories.

* (Part of distribution)
 * `lib` - R6RS-lite. Main library directory.
 * `lib-compat` - R6RS-lite. Implementation specific libraries.
 * `lib-r6rs` - R6RS. R7RS base libraries for R6RS. Renamed as `(scheme \*)`.
 * `lib-runtime` - Implementation specific format libraries. 
* (Built with run/buildstub.sh)
 * `lib-stub` - Implementation specific format. 
* (Part of yuni library development environment)
 * `lib-bootstrap` - Minimal proxy library to run run/buildstub.sh with nmosh

`run/\*.sh` describes which directories have to be used with each implementations.

Renaming
--------

Every library except under `lib` will be renamed before its use to provide consistent library namings. 

Library renaming rules are described at `config/config.scm`.

Syntax
------

Libraries in `lib` and `lib-compath` have to be written in R6RS-lite format; intersection of R6RS and R7RS lexical syntax. That mean;

* Do not use #vu8() or #u8(). These are not compatible between R6RS / R7RS.
* Do not use |(vertical bar) for symbol. Ditto.

SCHEME
======

Scheme implementation dependent notes.

R6RS/R7RS Hybrid
----------------

Since Yuni uses R7RS-small as Scheme base library, R6RS/R7RS hybrid implementations will be supported as first-class.

Several R6RS implements meta-level for library imports. So we need a proxy library to equalise levels.

* (r6rs-common-yuni compat macro primitives)
 * Low-level macro primitives

## Nmosh

Yuni uses Nmosh as its own reference implementation.

## Sagittarius

R6RS
----

## Racket

Racket requires `#!r6rs` for each R6RS styled library so we have to generate import stub for each libraries. See R7RS section below.

* SRFI-0 is not implemented at all on (racket-srfi i0)

## Guile

* SRFI-0 is not implemented at all on (guile-srfi i0)

## IronScheme

R7RS
----

## Chibi scheme

* Supported.

## Gauche

* Broken. Gauche cannot define any macro-defining-macro inside macro. J: http://d.hatena.ne.jp/mjt/20140914/p3

## picrin

## Foment

* No un-hygienic macro yet.

Others
------

## Chicken scheme

## Gambit


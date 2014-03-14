Porting Notes
-------------

In Yuni, every library written in:

* `.sls` extension
* R6RS style library definition(R6RS-light)

Obviously, we need some adaptation layer to support various Scheme implementatons. This file describes how do Yuni libraries adopts those implementations.

R6RS/R7RS Hybrid
----------------

Since Yuni uses R7RS-small as Scheme base library, R6RS/R7RS hybrid implementations will be supported as first-class.

## Nmosh

Yuni uses Nmosh as its own reference implementation. No stub library required.

## Sagittarius

R6RS
----

## Racket

 plt-r6rs.exe ++path lib-runtime/racket ++path lib-stub/racket PROGRAM.sps

Racket requires `#!r6rs` for each R6RS styled library so we have to generate import stub for each libraries. See R7RS section below.

## Guile

## IronScheme

## Vicare

R7RS
----

## Chibi scheme

## Gauche

## picrin

Others
------

## Chicken scheme

## Gambit


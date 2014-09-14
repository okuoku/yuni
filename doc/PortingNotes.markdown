Porting Notes
-------------

In Yuni, every library written in:

* `.sls` extension
* R6RS style library definition(R6RS-light)

Obviously, we need some adaptation layer to support various Scheme implementatons. This file describes how do Yuni libraries adopts those implementations.

See ../HACKING.markdown for generic library usage and standard commandline to develop the library itself.

R6RS/R7RS Hybrid
----------------

Since Yuni uses R7RS-small as Scheme base library, R6RS/R7RS hybrid implementations will be supported as first-class.

## Nmosh

Yuni uses Nmosh as its own reference implementation.

## Sagittarius

R6RS
----

## Racket

Racket requires `#!r6rs` for each R6RS styled library so we have to generate import stub for each libraries. See R7RS section below.

## Guile

## IronScheme

## Vicare

R7RS
----

## Chibi scheme

## Gauche

```
 gosh -r7 -I lib-runtime/r7rs -I lib-stub/gauche -A . PROGRAM.sps
```

## picrin

Others
------

## Chicken scheme

## Gambit


SIBR0013: Inexact negative zero handling
========================================

(Internal representation)

PositiveZero: `IronScheme`

(`eqv?` behaviour)

EquivalentPositiveZero: `Digamma` `Cyclone` `Bigloo` `s7`

(Number writer behaviour)

NonWritable: `SCM`

Some implementations cannot distinguish `-0.0` with `eqv?`.

```scheme
(eqv? 0.0 -0.0) ;; => #t/#f
```

Implementations other than `IronScheme` and `SCM` still prints "-0.0" or equivalent (e.g. "-0." on Gambit)
with `number->string` procedure.

`IronScheme` folds negative zero into positive zero, 

`SCM` internally distinguishes -0.0 but `write` procedure writes it as `"0."`. 

Workaround
==========

Every known implementations have consistent behaviour on `=` so it can be used where
result stability is important. This behaviour also complies IEEE754.

```scheme
(= -0.0 0.0) ;; => #t
```

It is NOT consistent on `equal?` or string comparision.

```scheme
(equal? -0.0 0.0) ;; => #t/#f
(string=? (number->string -0.0) (number->string 0.0)) ;; => #t/#f
```

Notes
=====

This SIBR may be divided into several capabilities such as number-reader/writer and arithmetics.

There was some discussion on `(abs -0.0)` bug:

* https://habr.com/en/post/574082/
* https://github.com/shirok/Gauche/commit/9a9b6357141c8b2444538563cebfb3547add1688

Generic `=` transitivity needs to be covered elsewhere:

* https://github.com/cisco/ChezScheme/issues/606
* https://github.com/ashinn/chibi-scheme/issues/812

Reference
=========

* https://github.com/okuoku/yuni/issues/163
* https://github.com/okuoku/yuni/issues/173
* https://docs.scheme.org/surveys/zero/

History
=======

Gauche adopted R7RS behaviour https://github.com/shirok/Gauche/commit/ca136f159390556a2b34dabf279ced88ab6edc89

STklos adopted R7RS behaviour https://github.com/egallesio/STklos/pull/585 (maybe)

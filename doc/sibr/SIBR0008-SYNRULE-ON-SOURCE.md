SIBR0008: Notation restriction on syntax-rules and define-syntax
================================================================

Affected: `Chicken`

Chicken requires `syntax-rules` should be written just after `define-syntax`
thus macro expands to `syntax-rules` cannot be used.

Workaround
==========

Yuni has integrated macro that expands into `define-syntax` and `syntax-rules`
at once.

```
(define-syntax define-syntax-rules/keywords
 (syntax-rules ()
  ((_ nam (symlit ...) (keylit ...) clauses ...)
   (define-syntax nam
    (syntax-rules (symlit ... keylit ...)
     clauses ...))))) 
```

Reference
=========

* http://wiki.call-cc.org/man/4/Macros

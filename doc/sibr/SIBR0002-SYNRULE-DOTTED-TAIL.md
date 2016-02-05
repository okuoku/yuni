SIBR0002: Syntax-rules does not accept dotted tail patttern
===========================================================

Affected: `Chicken`

`syntax-rules` may not accept dotted-tail pattern described as below.

```
(define-syntax lambda*1
 (syntax-rules ()
  ((_ sym (spec0 ... . last) body ...)
   (lambda*1-itr sym () () (spec0 ...) last body ...))))
```

Workaround
==========

Do not use the pattern. It is not allowed in the standard.


Reference
=========

* https://github.com/okuoku/yuni/issues/26


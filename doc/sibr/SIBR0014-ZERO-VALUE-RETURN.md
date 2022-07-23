SIBR0014: Cannot consume zero return values
===========================================

Affected: `s7`

s7 cannot consume zero return values due to its values handling.

```scheme
(call-with-values
  (lambda () (values))
  (lambda () 'bogus)) ;; => throws error
```

Workaround
==========

Receive unexpected with N-ary lambda. Implementations other than s7
will receive nil.


```scheme
(call-with-values
  (lambda () (values))
  (lambda a (length a)) ;; => 1 on s7, otherwise 0
```


Reference
=========

* https://github.com/okuoku/yuni/issues/167

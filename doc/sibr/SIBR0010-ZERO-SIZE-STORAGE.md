SIBR0010: Nil-like behavior with zero-length vector/string
==========================================================

Nil-like(string): `Digamma` `Chez`
Nil-like(vector): `Digamma` `Chez` `chibi-scheme`

Some Scheme implementations would treat zero-length vector and string just like
NIL; `eq?` is true between them.

```scheme
(define a (make-vector 0))
(define b (make-vector 0))

(eq? a b) ;; => #t/#f
```

For `nil-like` implemntations, `eq?` above returns `#t` just like `make-list` .

```scheme
(define a (make-list 0))
(define b (make-list 0))

(eq? a b) ;; => #t because these are nil
```

Workaround
==========

In case consistent behavior required between implementations, allocate
one-sized storage instead.

```scheme
(define a (make-vector 1))
(define b (make-vector 1))

(eq? a b) ;; => #f always
```

(import (yuni scheme)
        (yunitest mini)
        (yuni hashtables))

;;   make-symbol-hashtable
;;   make-eq-hashtable
;;   make-eqv-hashtable
;;   hashtable-ref
;;   hashtable-set!
;;   hashtable-entries

(define-syntax test-hashtable
  (syntax-rules ()
    ((_ const dummy-value (k0 v0) ...)
     (let ((ht (const)))
      ;; Empty
      (check-equal #f (hashtable-ref ht dummy-value #f))
      (check-equal #t (hashtable-ref ht dummy-value #t))
      (check-equal 123 (hashtable-ref ht dummy-value 123))
      (check-equal #\newline (hashtable-ref ht dummy-value #\newline))
      (check-equal 'NEVERLAND (hashtable-ref ht dummy-value 'NEVERLAND))
      (check-equal "Neverland" (hashtable-ref ht dummy-value "Neverland"))
      ;; Enter values
      (begin
        (hashtable-set! ht k0 v0)
        ...
        ;; Entries
        (call-with-values (lambda () (hashtable-entries ht))
                          (lambda (k v)
                            (check-equal (vector-length k) (length '(k0 ...)))
                            (check-equal (vector-length v) (length '(v0 ...)))
                            (for-each
                              (lambda (ke ve)
                                (check-equal ve (hashtable-ref ht ke dummy-value)))
                              (vector->list k)
                              (vector->list v)))))))))

(test-hashtable make-symbol-hashtable 'NEVERLAND)

(test-hashtable make-symbol-hashtable 'NEVERLAND
                ('a "a"))

(test-hashtable make-symbol-hashtable 'NEVERLAND
                ('a 1)
                ('b "2")
                ('c "1234")
                ((string->symbol "symbol with spaces") 123456)
                ('mm (eof-object)))

(define testht (make-symbol-hashtable))


(hashtable-set! testht 'a 'a)
(hashtable-set! testht 'b 'b)
(hashtable-set! testht 'c 'c)

(let ((v (hashtable-keys testht)))
 (check-equal #t (vector? v))
 (check-equal 3 (vector-length v))
 (vector-for-each (lambda (k)
                    (check-equal k (hashtable-ref testht k #f)))
                  v))

(test-hashtable make-eq-hashtable
                'NEVERLAND
                (0 1)
                (-1 "m")
                (1 2)
                (3 4)
                ("a" 6)
                ("b" 7)
                (0.0 8))

(test-hashtable make-eqv-hashtable
                'NEVERLAND
                (0 1)
                (-1 "m")
                (1 2)
                (3 4)
                ("a" 5)
                ("b" 6)
                (0.0 7))

(let ((a 0.0)
      (ht (make-eqv-hashtable)))
  (hashtable-set! ht a 'a)
  (check-equal 'a (hashtable-ref ht a #f))
  (hashtable-set! ht 0.0 'b)
  (check-equal 'b (hashtable-ref ht a #f))
  (check-equal 'b (hashtable-ref ht 0.0 #f)))

(check-finish)

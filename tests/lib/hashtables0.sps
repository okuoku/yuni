(import (yuni scheme)
        (yunitest mini)
        (yuni hashtables))

;; FIXME: Currently it's only for lighteval
;; 
;;   make-symbol-hashtable
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

(check-finish)

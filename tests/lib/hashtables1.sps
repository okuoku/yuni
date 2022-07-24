(import (yuni scheme)
        (yunitest mini)
        (yuni hashtables))

;; Use fixnum for hashtable key
;; https://github.com/okuoku/yuni/issues/144

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

(test-hashtable make-eq-hashtable
                'NEVERLAND
                (0 1)
                (-1 "m")
                (1 2)
                (3 4)
                ("a" 5)
                ("b" 6)
                (0.0 7))

(check-finish)

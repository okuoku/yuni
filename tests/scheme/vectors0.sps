(import (yuni scheme)
        (yunitest mini))

(define-syntax copytest
  (syntax-rules ()
    ((_ copy (copyproc! obj at ...))
     (let ((target (copy obj)))
      (copyproc! target at ...)
      target))))

;; Vectors

(check-equal '#() (vector))
(check-equal '#(1) (vector 1))
(check-equal '#(10 20) (vector 10 20))
(check-equal '#(10 20 30) (vector 10 20 30))
(check-equal '() (vector->list '#()))
(check-equal '() (vector->list (make-vector 0 #\a)))
(check-equal '(10) (vector->list (make-vector 1 10)))
(check-equal '(10 20 30) (vector->list '#(10 20 30)))
(check-equal "" (vector->string (make-vector 0)))
(check-equal "a" (vector->string '#(#\a)))
(check-equal "abc" (vector->string '#(#\a #\b #\c)))
(check-equal "abc" (vector->string '#(#\0 #\0 #\a #\b #\c) 2))
(check-equal "abc" (vector->string '#(#\0 #\0 #\a #\b #\c) 2 5))
(check-equal "" (vector->string '#(#\0 #\0 #\0) 1 1))
(check-equal '#() (string->vector ""))
(check-equal '#() (string->vector "abc" 1 1))
(check-equal '#(#\a) (string->vector "a"))
(check-equal '#(#\a #\b #\c) (string->vector "00abc" 2))
(check-equal '#(#\a #\b #\c) (string->vector "00abc00" 2 5))
(check-equal '#() (vector-copy (make-vector 0)))
(check-equal '#(10 20 30) (vector-copy (vector 10 20 30)))
(check-equal '#() (vector-copy (vector 10 20 30) 1 1))
(check-equal '#(20) (vector-copy (vector 10 20 30) 1 2))
(check-equal '#(20 30) (vector-copy (vector 10 20 30) 1))
(check-equal '#(20 30) (vector-copy (vector 10 20 30) 1 3))
(check-equal '#(10 20 30) (copytest vector-copy
                                    (vector-copy! (make-vector 3)
                                                  0
                                                  (vector 10 20 30))))
(check-equal '#(10) (copytest vector-copy
                              (vector-copy! (make-vector 1)
                                            0
                                            (vector 10 20 30)
                                            0 1)))
(check-equal '#(20 30) (copytest vector-copy
                                 (vector-copy! (make-vector 2)
                                               0
                                               (vector 10 20 30)
                                               1)))
(check-equal '#(10 20 30 40) (copytest vector-copy
                                       (vector-copy! (make-vector 4 10)
                                                    1
                                                    (vector 20 30 40))))

(check-equal '#(10 20 30) (vector-append (vector 10) (vector 20 30)))
(check-equal '#(10 20 30) (vector-append (vector 10 20 30)))
(check-equal '#(10 20 30) (vector-append (vector) (vector) (vector 10 20 30)
                                         (vector)))

(check-equal '#(10 10 10) (copytest vector-copy
                                    (vector-fill! (make-vector 3) 10)))
(check-equal '#(10 20 20) (copytest vector-copy
                                    (vector-fill! (make-vector 3 10) 20 1)))
(check-equal '#(10 20 10) (copytest vector-copy
                                    (vector-fill! (make-vector 3 10) 20 1 2)))
(check-equal '#() (copytest vector-copy
                            (vector-fill! (vector) 10)))

(let ((v (make-vector 1)))
 (vector-set! v 0 (lambda () 1234))
 (check-equal 1234 ((vector-ref v 0))))

(check-finish)

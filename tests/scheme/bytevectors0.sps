(import (yuni scheme)
        (yunitest mini))

(define-syntax copytest
  (syntax-rules ()
    ((_ copy (copyproc! obj at ...))
     (let ((target (copy obj)))
      (copyproc! target at ...)
      target))))


;; Bytevectors

(check-equal (make-bytevector 0) (bytevector))
(check-equal (make-bytevector 2 10) (bytevector 10 10))
(check-equal (make-bytevector 2 10) (bytevector-copy (bytevector 10 10)))
(check-equal (bytevector 20 20 20) (bytevector-copy (bytevector 10 10 20
                                                                20 20)
                                                    2))
(check-equal (bytevector 10 20) (bytevector-copy (bytevector 10 10 20 20 20
                                                             20)
                                                 1 3))
(check-equal (bytevector 10 20 30 40 50)
             (copytest bytevector-copy
                       (bytevector-copy! (bytevector 10 10 10 10 10)
                                         1
                                         (bytevector 20 30 40 50))))
(check-equal (bytevector 10 20 30 40 10)
             (copytest bytevector-copy
                       (bytevector-copy! (make-bytevector 5 10)
                                         1
                                         (bytevector 20 30 40 50)
                                         0 3)))
(check-equal (bytevector 10 20 30 40 50)
             (bytevector-append (bytevector)
                                (bytevector 10 20)
                                (bytevector)
                                (bytevector 30)
                                (bytevector 40 50)))

(check-equal "abc" (utf8->string (bytevector 97 98 99)))
(check-equal (bytevector 97 98 99) (string->utf8 "abc"))
(check-equal (bytevector 99) (string->utf8 "abc" 2))
(check-equal (bytevector 98) (string->utf8 "abc" 1 2))
(check-equal (bytevector) (string->utf8 "abc" 1 1))
(check-equal "" (utf8->string (bytevector)))
(check-equal (bytevector) (string->utf8 ""))


(check-equal "abc" (utf8->string (bytevector 97 98 99) 0))
(check-equal "c" (utf8->string (bytevector 97 98 99) 2))
(check-equal "b" (utf8->string (bytevector 97 98 99) 1 2))
(check-equal "" (utf8->string (bytevector 97 98 99) 1 1))


(let ((bv1 (bytevector 1 2 3 4 5 6 7 8))
      (bv2 (bytevector 99 99 99)))
  (bytevector-copy! bv2 1 bv1 6)
  (check-equal (bytevector 99 7 8) bv2))

(check-finish)

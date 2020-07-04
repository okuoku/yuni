;; Exceptions
(import (yuni scheme)
        (yunitest mini))

;; with-exception-handler (success)
(check-equal 'a
             (with-exception-handler
               (lambda _ (error "huh?"))
               (lambda () 'a)))

(call-with-values
  (lambda ()
    (with-exception-handler
      (lambda _ 'ok)
      (lambda () (values))))
  (lambda v
    (check-equal '() v)))

;; with-exception-handler (raise)
(define hook0 #f)
(check-equal
  102
  (with-exception-handler
    (lambda (obj)
      (set! hook0 "check")
      (check-equal "raise" obj)
      100)
    (lambda ()
      (+ 1 (raise-continuable "raise") 1))))
(check-equal hook0 "check")

;; guard
#|
(check-equal
  'abcd
  (guard
    (cnd
      (#t
       (check-equal #t (error-object? cnd))
       (check-equal "errmsg" (error-object-message cnd))
       (check-equal '(1234) (error-object-irritants cnd))
       'abcd))
    (error "errmsg" 1234)
    5678))
|#

(check-equal
  1234
  (guard
    (bogus
      (#f 'bogus)
      (else 'bogus))
    1234))


(check-finish)

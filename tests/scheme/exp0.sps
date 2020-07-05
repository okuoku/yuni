;; Exceptions
(import (yuni scheme)
        (yunitest mini))

#|
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
|#

;; guard
(check-equal
  'abcd
  (guard
    (cnd
      (#t 'abcd)
      (else 'bogus))
    (error "errormsg" 1234)
    5678))

(check-equal ;; `raise`ed object may not be an error-object
  #f
  (guard
    (c
      ((error-object? c) c)
      (else #f))
    (raise "bogus")))

(check-equal
  1234
  (guard
    (cnd
      (#f 'bogus)
      (else 1234))
    (error "errmsg" 1234)
    5678))

(check-equal
  12
  (guard
    (cnd
      ((char? cnd) 'bogus))
    (guard
      (cnd
        ((error-object? cnd) 12)
        (else 'bogus2))
      (error "errormsg" 1234))))


(check-equal ;; implicit raise-continuable case
  34
  (guard
    (cnd
      ((char? cnd) 34))
    (guard
      (cnd
        ((error-object? cnd) 'bogus))
      (raise #\e))))

(check-equal
  1234
  (guard
    (bogus
      (#f 'bogus)
      (else 'bogus))
    1234))

(let ((peek #f)) ;; Emulate try-catch-finally with re-raise caught exception
 (check-equal
   5678
   (guard
     (_
       (#t 5678))
     (guard
       (cnd
         (#t
          (set! peek #t)
          (raise cnd)))
       (error "errormsg" 1234))))
 (check-equal #t peek))

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


(check-finish)

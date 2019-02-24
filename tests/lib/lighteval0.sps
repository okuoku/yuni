(import (yuni scheme)
        (yuni lighteval)
        (yunitest mini))

(define env0 (make-lighteval-env))
(define env1 (make-lighteval-env))

;; Simple set/ref

(check-equal 'NEVERLAND
             (lighteval-env-ref env0 'sym 'NEVERLAND))

(lighteval-env-set! env0 'sym 0)

(check-equal 0 (lighteval-env-ref env0 'sym 'NEVERLAND))

;; Define(even/odd)

(lighteval-env-add-globals! env1
                            '((xodd? . (lambda (i) (if (= i 0) 
                                                     #f
                                                     (xeven? (- i 1)))))
                              (xeven? . (lambda (i) (if (= i 0)
                                                      #t
                                                      (xodd? (- i 1)))))))

(let ((xodd? (lighteval-env-ref env1 'xodd? #f))
      (xeven? (lighteval-env-ref env1 'xeven? #f)))
  (check-equal #t (xeven? 0))
  (check-equal #t (xeven? 10))
  (check-equal #f (xeven? 9))
  (check-equal #f (xodd? 0))
  (check-equal #f (xodd? 10))
  (check-equal #t (xodd? 9)))

;; Eval
(check-equal '#(#t #t #f #f #f #t) 
             (lighteval-bind env1
                             '(vector
                                (xeven? 0)
                                (xeven? 10)
                                (xeven? 9)
                                (xodd? 0)
                                (xodd? 10)
                                (xodd? 9))))

(check-finish)

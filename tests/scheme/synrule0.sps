(import (yuni scheme)
        (yunitest mini))

(define-syntax chk
  (syntax-rules ()
    ((_ nam val)
     (begin 
       ;; `tmp` should be renamed
       (define tmp val)
       (define (nam) tmp)))))

(chk a 10)
(chk b 20)

(check-equal (a) 10)
(check-equal (b) 20)

(check-finish)

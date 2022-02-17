(import (yuni scheme)
        (yunitest mini))

(define-syntax chk0
  (syntax-rules ()
    ((_ nam val)
     (begin 
       (define nam val)))))

(define-syntax chk1
  (syntax-rules ()
    ((_ nam val)
     (define nam val))))

(chk0 a 10)
(chk1 b 20)

(check-equal a 10)
(check-equal b 20)

(let ((a 30))
  (let ()
   (chk0 a 40)
   (check-equal a 40))
  (check-equal a 30))

(check-finish)

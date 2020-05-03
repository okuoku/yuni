(import (yuni scheme)
        (yunitest mini)
        (yuni io drypack))

(define (string->obj str)
  (let* ((p (open-input-string str))
         (q (open-input-string (read p))))
   (read q)))

(define (obj->string obj)
  (let ((p (open-output-string)))
   (get-output-string p)))

(define (decode bv)
  (let ((p (open-input-bytevector bv)))
   (drypack-get p)))

(define (encode obj)
  (let ((p (open-output-bytevector)))
   (drypack-put p obj)
   (get-output-bytevector p)))

(define (loopback obj)
  ;; FIXME: Can't this be portable??
  (let* ((objstring (obj->string obj))
         (stringobj (string->obj objstring)))
    (check-equal objstring stringobj)
    (check-equal obj (decode (encode obj)))
    (check-equal obj (decode (encode objstring)))))

(define (loopback0 obj)
  (check-equal obj (decode (encode obj))))

;; Single objects

(check-equal #t (decode (encode #t))) ;; Zone0
(check-equal #f (decode (encode #f))) ;; Zone0
(check-equal '() (decode (encode '()))) ;; Zone0
(check-equal (eof-object) (decode (encode (eof-object)))) ;; Zone0

(check-equal #\\ (decode (encode #\\ )))
(check-equal #\a (decode (encode #\a)))

(check-equal 123456 (decode (encode 123456)))
(check-equal -1234567 (decode (encode -1234567)))
(check-equal 0.0 (decode (encode 0.0)))

(check-equal 'equal? (decode (encode 'equal?)))
(check-equal 'a (decode (encode 'a)))

(check-equal "" (decode (encode "")))
(check-equal "abc" (decode (encode "abc")))

(check-equal (bytevector) (decode (encode (bytevector))))
(check-equal (bytevector 1 2 3 4) (decode (encode (bytevector 1 2 3 4))))
(check-equal (bytevector #x81 #x92 #xa3 #xb4) (decode (encode (bytevector #x81 #x92 #xa3 #xb4))))

;; Containers
(loopback0 '#(a b c "hoge" #\a #(1 2 3 4)))
(loopback0 '(a))
(loopback0 '(a . b))
(loopback0 '(a . "x"))

(let ((p (cons #f #f)))
 (set-cdr! p p)
 (let* ((bv (encode p))
        (x (decode bv)))
   (check-equal #t (eq? x (cdr x)))))

(let ((v (vector 0 0 0))
      (v2 (vector "1" "2" "3")))
  (vector-set! v 0 v2)
  (vector-set! v 2 v)
  (let* ((bv (encode v))
         (x (decode bv)))
    (check-equal v2 (vector-ref v 0))
    (check-equal 0 (vector-ref v 1))
    (check-equal #t (eq? x (vector-ref x 2)))))

(check-finish)

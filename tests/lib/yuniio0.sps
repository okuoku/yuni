(import (yuni scheme)
        (yunitest mini)
        (yuni io leb128))

(define (encode num)
  (let ((p (open-output-bytevector)))
   (leb128-put p num)
   (get-output-bytevector p)))

(define (decode bv)
  (let ((p (open-input-bytevector bv)))
    (leb128-get p)))

(define (loopback/leb128 n)
  (check-equal n (decode (encode n))))

(check-equal (bytevector 0) (encode 0))
(check-equal 0 (decode (bytevector 0)))

(check-equal (bytevector 127) (encode 127))
(check-equal 127 (decode (bytevector 127)))

;; Wikipedia example: https://en.wikipedia.org/wiki/LEB128
(check-equal (bytevector #xe5 #x8e #x26) (encode 624485))

(loopback/leb128 0)
(loopback/leb128 1)
(loopback/leb128 10)
(loopback/leb128 100)
(loopback/leb128 1000)
(loopback/leb128 10000)
(loopback/leb128 #x10)
(loopback/leb128 #x100)
(loopback/leb128 #x1000)
(loopback/leb128 #x10000)
(loopback/leb128 #xff)
(loopback/leb128 #xfff)
(loopback/leb128 #xffff)
(loopback/leb128 #xfffff)

(check-finish)

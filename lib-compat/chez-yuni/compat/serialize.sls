(library (chez-yuni compat serialize)
         (export
           put-object/serialize
           get-object/deserialize
           serialize-object
           deserialize-object)
         (import (only (chezscheme)
                       fasl-write
                       fasl-read)
                 (yuni scheme))

;;

(define (put-object/serialize port obj)
  (fasl-write obj port))
(define (get-object/deserialize port)
  (fasl-read port))

(define (serialize-object obj) ;; => bv
  (let ((p (open-output-bytevector)))
   (fasl-write obj p)
   (let ((bv (get-output-bytevector p)))
    (close-port p)
    bv)))

(define (deserialize-object bv)
  (let ((p (open-input-bytevector bv)))
   (let ((obj (fasl-read p)))
    (close-port p)
    obj))) 
)

(library (yuni ffi runtime bootstraploader)
         (export
           make-bootstraploader
           )
         (import (yuni scheme)
                 (yuni compat bitwise primitives))

(define YUNIBOOTSTRAP0_MALLOC 1)
(define YUNIBOOTSTRAP0_FREE 2)
(define YUNIBOOTSTRAP0_DLOPEN 3)
(define YUNIBOOTSTRAP0_DLSYM 4)
         
(define (make-bootstraploader nccc-call bootstrap
                              ptr-write/asciiz!
                              integer->ptr
                              ) ;; => dlopen dlsym
  (define bootstrap-ptr (bootstrap))
  (define (call-bootstrap arg0 arg1 arg2 arg3) ;; => (out0 . out1)
    (define in (make-bytevector (* 8 4)))
    (define out (make-bytevector (* 8 2)))
    ;; Fill in packet
    (bv-write/u64! in (* 8 0) arg0)
    (bv-write/u64! in (* 8 1) arg1)
    (bv-write/u64! in (* 8 2) arg2)
    (bv-write/u64! in (* 8 3) arg3)
    ;; Call
    (nccc-call bootstrap-ptr in 0 4 out 0 2)
    ;; Read out packet
    (let ((out0 (bv-read/u64 out 0))
          (out1 (bv-read/u64 out 8)))
      (cons out0 out1)))
  (define (malloc siz)
    (let ((p (call-bootstrap YUNIBOOTSTRAP0_MALLOC siz 0 0)))
     (car p)))
  (define (free ptr)
    (call-bootstrap YUNIBOOTSTRAP0_FREE ptr 0 0))
  (define (dlopen path)
    (define namebuf (malloc 4096))
    (ptr-write/asciiz! (integer->ptr namebuf) 0 4096 path)
    (let ((p (call-bootstrap YUNIBOOTSTRAP0_DLOPEN namebuf 0 0)))
     (free namebuf)
     (let ((err (car p))
           (ptr (cdr p)))
       (and (= err 0)
            ptr))))
  (define (dlsym ptr nam)
    (define namebuf (malloc 4096))
    (ptr-write/asciiz! (integer->ptr namebuf) 0 4096 nam)
    (let ((p (call-bootstrap YUNIBOOTSTRAP0_DLSYM ptr namebuf 0)))
     (free namebuf)
     (let ((err (car p))
           (ptr (cdr p)))
       (and (= err 0)
            (integer->ptr ptr)))))
  (values dlopen dlsym))
         
)

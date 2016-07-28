(library (yuni ffi runtime bootstraploader)
         (export make-bootstraploader)
         (import (yuni scheme)
                 (yuni compat bitwise primitives))

(define YUNIBOOTSTRAP0_MALLOC 1)
(define YUNIBOOTSTRAP0_FREE 2)
(define YUNIBOOTSTRAP0_DLOPEN 3)
(define YUNIBOOTSTRAP0_DLSYM 4)
         
(define (make-bootstraploader nccc-call bootstrap
                              ptr?
                              bv-read/w64ptr
                              bv-write/w64ptr!
                              ptr-write/asciiz!
                              ) ;; => dlopen dlsym
  (define bootstrap-ptr (bootstrap))
  (define (writeword bv off arg)
    (if (ptr? arg)
      (bv-write/w64ptr! bv off arg)
      (bv-write/u64! bv off arg)))
  (define (call-bootstrap proccar proccdr 
                          arg0 arg1 arg2 arg3) ;; => (out0 . out1)
    (define in (make-bytevector (* 8 4)))
    (define out (make-bytevector (* 8 2)))
    ;; Fill in packet
    (writeword in (* 8 0) arg0)
    (writeword in (* 8 1) arg1)
    (writeword in (* 8 2) arg2)
    (writeword in (* 8 3) arg3)
    ;; Call
    (nccc-call bootstrap-ptr in 0 4 out 0 2)
    ;; Read out packet
    (let ((out0 (and proccar (proccar out 0)))
          (out1 (and proccdr (proccdr out 8))))
      (cons out0 out1)))
  (define (malloc siz)
    (let ((p (call-bootstrap bv-read/w64ptr #f YUNIBOOTSTRAP0_MALLOC siz 0 0)))
     (car p)))
  (define (free ptr)
    (call-bootstrap #f #f YUNIBOOTSTRAP0_FREE ptr 0 0))
  (define (dlopen path)
    (define namebuf (malloc 4096))
    (ptr-write/asciiz! namebuf 0 4096 path)
    (let ((p (call-bootstrap bv-read/u64 bv-read/w64ptr
                             YUNIBOOTSTRAP0_DLOPEN namebuf 0 0)))
     (free namebuf)
     (let ((err (car p))
           (ptr (cdr p)))
       (and (= err 0)
            ptr))))
  (define (dlsym ptr nam)
    (define namebuf (malloc 4096))
    (ptr-write/asciiz! namebuf 0 4096 nam)
    (let ((p (call-bootstrap bv-read/u64 bv-read/w64ptr
                             YUNIBOOTSTRAP0_DLSYM ptr namebuf 0)))
     (free namebuf)
     (let ((err (car p))
           (ptr (cdr p)))
       (and (= err 0)
            ptr))))
  (values dlopen dlsym))
         
)

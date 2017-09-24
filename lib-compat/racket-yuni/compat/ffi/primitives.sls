(library (racket-yuni compat ffi primitives)
         (export yuniffi-nccc-call
                 yuniffi-nccc-ptr->callable
                 yuniffi-module-load
                 yuniffi-module-lookup

                 ;; Memory OPs (pointers)
                 ptr?
                 ptr-read/w64ptr
                 ptr-read/s8 ptr-read/u8 ptr-read/s16 ptr-read/u16
                 ptr-read/s32 ptr-read/u32 ptr-read/s64 ptr-read/u64
                 ptr-read/asciiz
                 ptr-write/s8! ptr-write/u8! ptr-write/s16! ptr-write/u16!
                 ptr-write/s32! ptr-write/u32! ptr-write/s64! ptr-write/u64!
                 ptr-write/asciiz!

                 bv-read/w64ptr
                 bv-write/w64ptr!
                 )
         (import (yuni scheme)
                 ;; Some of definitions are placed in runtime
                 ;; because we need to use keywords and other Racket specific
                 ;; notations.
                 (rename
                   (only (racket base) 
                         map
                         list->vector
                         path->string
                         current-library-collection-paths)
                   (map map:racket)
                   (list->vector list->vector:racket)
                   )
                 (yuni-runtime racket-ffi)
                 (yuni ffi runtime simpleloader)
                 (yuni ffi runtime simplestrings)
                 (ffi unsafe))

;;

;; Unfortunately, Racket also uses `ptr` prefix.
(define (ptr? x) (and (cpointer? x)
                      ;; In Racket, bytevector(byte strings) and 
                      ;; #f(treated as NULL) are also cpointer.
                      ;; We exclude them here for now.
                      (not (bytevector? x))
                      (not (eq? #f x))))

(define (ptr-read/s8 p off) (ptr-ref p _int8 off))
(define (ptr-read/u8 p off) (ptr-ref p _uint8 off))
(define (ptr-read/s16 p off) (ptr-ref p _int16 off))
(define (ptr-read/u16 p off) (ptr-ref p _uint16 off))
(define (ptr-read/s32 p off) (ptr-ref p _int32 off))
(define (ptr-read/u32 p off) (ptr-ref p _uint32 off))
(define (ptr-read/s64 p off) (ptr-ref p _int64 off))
(define (ptr-read/u64 p off) (ptr-ref p _uint64 off))
(define (ptr-write/s8! p off v) (ptr-set! p _int8 off v))
(define (ptr-write/u8! p off v) (ptr-set! p _uint8 off v))
(define (ptr-write/s16! p off v) (ptr-set! p _int16 off v))
(define (ptr-write/u16! p off v) (ptr-set! p _uint16 off v))
(define (ptr-write/s32! p off v) (ptr-set! p _int32 off v))
(define (ptr-write/u32! p off v) (ptr-set! p _uint32 off v))
(define (ptr-write/s64! p off v) (ptr-set! p _int64 off v))
(define (ptr-write/u64! p off v) (ptr-set! p _uint64 off v))

;; FIXME: Assumes little-endian
;; defined in (yuni-runtime racket-ffi)
(define (ptr-read/w64ptr p off)
  (ptr-ref/cpointer p off))
(define (ptr-write/w64ptr! p off v)
  (ptr-set/cpointer! p off v))
(define (bv-read/w64ptr bv off)
  (ptr-ref/cpointer bv off))
(define (bv-write/w64ptr! bv off v)
  (ptr-set/cpointer! bv off v))

(define-read-asciiz ptr-read/asciiz ptr-read/u8)
(define-write-asciiz ptr-write/asciiz! ptr-write/u8!)

(define (yuniffi-nccc-call func 
                           in in-offset in-size
                           out out-offset out-size) 
  ;; Bytevector is byte string in Racket. So we don't have to convert them.
  (define inp (and in (ptr-add in (* in-offset 8))))
  (define outp (and out (ptr-add out (* out-offset 8))))
  (func inp in-size outp out-size))

(define (yuniffi-nccc-ptr->callable ptr)
  (cast ptr _pointer nccc-func))

(define (yuniffi-module-lookup handle str)
  ;; nccc-func is defined in (yuni-runtime racket-ffi)
  (get-ffi-obj str handle nccc-func
               ;; Failure thunk
               (lambda () #f)))

(define (module-path) 
  (vector->list
    (list->vector:racket 
      (map:racket path->string
                  (current-library-collection-paths)))))
         
(define (module-load path)
  (guard (c (#t #f))
         (ffi-lib path)))

(define yuniffi-module-load (make-simpleloader module-path module-load))

)

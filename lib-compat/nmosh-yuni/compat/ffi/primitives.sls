(library (nmosh-yuni compat ffi primitives)
         (export yuniffi-nccc-call
                 yuniffi-nccc-patchcall
                 yuniffi-module-load
                 yuniffi-module-lookup

                 ;; Memory OPs (pointers)
                 ptr? integer->ptr
                 ptr-read/s8 ptr-read/u8 ptr-read/s16 ptr-read/u16
                 ptr-read/s32 ptr-read/u32 ptr-read/s64 ptr-read/u64
                 ptr-read/asciiz
                 ptr-write/s8! ptr-write/u8! ptr-write/s16! ptr-write/u16!
                 ptr-write/s32! ptr-write/u32! ptr-write/s64! ptr-write/u64!
                 ptr-write/asciiz!
                 )
         (import (yuni scheme)
                 (yuni ffi runtime simplepatcher)
                 (yuni ffi runtime simpleloader)
                 (yuni ffi runtime simplestrings)
                 (yuni compat bitwise primitives)
                 (only (mosh ffi)
                       lookup-shared-library ;; (%ffi-lookup handle sym)
                       close-shared-library  ;; (%ffi-close handle)
                       shared-library-error  ;; (%ffi-error)
                       open-shared-library   ;; (%ffi-open path)
                       integer->pointer
                       pointer->integer
                       pointer?
                       pointer-ref-c-int8 pointer-ref-c-uint8
                       pointer-ref-c-int16 pointer-ref-c-uint16
                       pointer-ref-c-int32 pointer-ref-c-uint32
                       pointer-ref-c-int64 pointer-ref-c-uint64
                       pointer-set-c-int8! pointer-set-c-uint8!
                       pointer-set-c-int16! pointer-set-c-uint16!
                       pointer-set-c-int32! pointer-set-c-uint32!
                       pointer-set-c-int64! pointer-set-c-uint64!
                       )
                 (yuni-nmosh primitives)
                 (nmosh global-flags))

;; 

(define (ptr? x) (pointer? x))
(define (integer->ptr x) (integer->pointer x))

(define (ptr-read/s8 x off) (pointer-ref-c-int8 x off))
(define (ptr-read/u8 x off) (pointer-ref-c-uint8 x off))
(define (ptr-read/s16 x off) (pointer-ref-c-int16 x off))
(define (ptr-read/u16 x off) (pointer-ref-c-uint16 x off))
(define (ptr-read/s32 x off) (pointer-ref-c-int32 x off))
(define (ptr-read/u32 x off) (pointer-ref-c-uint32 x off))
(define (ptr-read/s64 x off) (pointer-ref-c-int64 x off))
(define (ptr-read/u64 x off) (pointer-ref-c-uint64 x off))
(define (ptr-write/s8! x off v) (pointer-set-c-int8! x off v))
(define (ptr-write/u8! x off v) (pointer-set-c-uint8! x off v))
(define (ptr-write/s16! x off v) (pointer-set-c-int16! x off v))
(define (ptr-write/u16! x off v) (pointer-set-c-uint16! x off v))
(define (ptr-write/s32! x off v) (pointer-set-c-int32! x off v))
(define (ptr-write/u32! x off v) (pointer-set-c-uint32! x off v))
(define (ptr-write/s64! x off v) (pointer-set-c-int64! x off v))
(define (ptr-write/u64! x off v) (pointer-set-c-uint64! x off v))

(define-read-asciiz ptr-read/asciiz ptr-read/u8)
(define-write-asciiz ptr-write/asciiz! ptr-write/u8!)

;; 

(define %nmosh-yuniffi-call-nccc
  (let ((f (get-global-flag '%nmosh-yuniffi-call-nccc)))
   (if f f (lambda e (error "yuniFFI support is not compiled-in!"))))) 

(define *empty* (make-bytevector 0))

(define (yuniffi-nccc-call func 
                           in in-offset in-size
                           out out-offset out-size)
  (define xin (or in *empty*))
  (define xout (or out *empty*))
  (when (and (not in) (not (= in-size 0)))
    (error "in-size is not zero" in-size))
  (when (and (not out) (not (= out-size 0)))
    (error "out-size is not zero" out-size))

  (%nmosh-yuniffi-call-nccc
    func
    xin in-offset in-size
    xout out-offset out-size))

(define (y-bytevector->pointer bv offs)
  (let ((len (bytevector-length bv)))
    (when (>= offs len)
      (error "offset beyonds bytevector length" offs))
   (let ((idx (+ offs (pointer->integer (bytevector-pointer bv)))))
    (integer->pointer idx))))

(define (set-pointer! bv offs ptr)
  (bv-write/u64! bv offs (pointer->integer ptr)))

(define (offset-pointer ptr offs)
  (integer->pointer (+ offs (pointer->integer ptr))))

(define-simplepatcher
  yuniffi-nccc-patchcall
  yuniffi-nccc-call
  y-bytevector->pointer
  set-pointer!
  offset-pointer)

(define (module-load path) ;; => handle/#f
  (guard (c (#t #f))
         (open-shared-library path)))

(define (module-path) prefix-list)

(define yuniffi-module-load (make-simpleloader module-path module-load))

(define (yuniffi-module-lookup handle str)
  (lookup-shared-library handle (string->symbol str)))

)

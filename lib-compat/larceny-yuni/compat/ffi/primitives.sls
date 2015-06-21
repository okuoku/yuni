(library (larceny-yuni compat ffi primitives)
         (export
           yuniffi-nccc-call
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
                 (yuni ffi runtime simpleloader)
                 (yuni ffi runtime simplestrings)
                 (yuni compat bitwise primitives)
                 ;; FIXME: Move them into runtime
                 (primitives
                   void*-rt
                   peek-bytes
                   poke-bytes
                   record-accessor
                   record-predicate
                   record-constructor
                   current-require-path
                   sizeof:pointer
                   ffi/handle->address
                   foreign-file
                   foreign-procedure))


;; pointer procedures
(define ptr? (record-predicate void*-rt))
(define (integer->ptr x) ((record-constructor void*-rt) x))

;; FIXME: Inefficient.
(define (ptr->integer x) ((record-accessor void*-rt 'ptr) x))
(define-syntax defop
  (syntax-rules ()
    ((_ nam siz bop #f)
     ;; Read op
     (define (nam x off)
       (let ((bv (make-bytevector siz)))
        (peek-bytes (+ (ptr->integer x) off) bv siz)
        (bop bv 0))))
    ((_ nam siz bop #t)
     ;; Write op
     (define (nam x off v)
       (let ((bv (make-bytevector siz)))
        (bop bv 0 v)
        (poke-bytes (+ off (ptr->integer x)) bv siz))))))

(defop ptr-read/s8 1 bv-read/s8 #f)
(defop ptr-read/u8 1 bv-read/u8 #f)
(defop ptr-read/s16 2 bv-read/s16 #f)
(defop ptr-read/u16 2 bv-read/u16 #f)
(defop ptr-read/s32 4 bv-read/s32 #f)
(defop ptr-read/u32 4 bv-read/u32 #f)
(defop ptr-read/s64 8 bv-read/s64 #f)
(defop ptr-read/u64 8 bv-read/u64 #f)

(defop ptr-write/s8! 1 bv-write/s8! #t)
(defop ptr-write/u8! 1 bv-write/u8! #t)
(defop ptr-write/s16! 2 bv-write/s16! #t)
(defop ptr-write/u16! 2 bv-write/u16! #t)
(defop ptr-write/s32! 4 bv-write/s32! #t)
(defop ptr-write/u32! 4 bv-write/u32! #t)
(defop ptr-write/s64! 8 bv-write/s64! #t)
(defop ptr-write/u64! 8 bv-write/u64! #t)

(define-read-asciiz ptr-read/asciiz ptr-read/u8)
(define-write-asciiz ptr-write/asciiz! ptr-write/u8!)

;; NB: It seems Larceny's FFI do not support any namespacing..

(define (yuniffi-nccc-call func 
                           in in-offset in-size 
                           out out-offset out-size)
  ;; NB: We assume 'in' and 'out' are automagically GC-protected.
  ;; NB: It seems there is no public function for ffi/handle->address
  ;; Ref:
  ;;   larceny/lib/Ffi/memory.sch
  ;;   larceny/src/Rts/Sys/syscall.c
  ;;   larceny/include/Sys/macros.h
  (define in-addr (+ (* sizeof:pointer (+ 1 in-offset)) ;; 1 = Skip header
                     (ffi/handle->address in)))
  (define out-addr (+ (* sizeof:pointer (+ 1 out-offset)) ;; 1 = Skip header
                      (ffi/handle->address out)))
  (func in-addr in-size out-addr out-size))

(define (module-load path)
  ;; FIXME: No way to detect error..?
  (and (file-exists? path)
       ;; No FFI module handle on Larceny
       (begin (foreign-file path)
              #t)))

(define (yuniffi-module-lookup handle str) ;; => procedure
  ;; FIXME: Wow, no pointer type! Seriously?
  ;;    NB: We cannot use boxed type because we have to offset pointer values
  (foreign-procedure str '(int int int int) 'void))

(define (module-path) (current-require-path))
(define yuniffi-module-load (make-simpleloader module-path module-load))
         
)

(library (vicare-yuni compat ffi primitives)
         (export yuniffi-nccc-call
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
                 (only (vicare)
                       bytevector->memory
                       pointer-add
                       )
                 (only (vicare libraries)
                       library-source-search-path)
                 (yuni ffi runtime simpleloader)
                 (yuni ffi runtime simplestrings)
                 (vicare ffi))
;; A bit different from nmosh
(define (ptr? x) (pointer? x))
(define (integer->ptr x) (integer->pointer x))
 
(define (ptr-read/s8 x off) (pointer-ref-c-sint8 x off))
(define (ptr-read/u8 x off) (pointer-ref-c-uint8 x off))
(define (ptr-read/s16 x off) (pointer-ref-c-sint16 x off))
(define (ptr-read/u16 x off) (pointer-ref-c-uint16 x off))
(define (ptr-read/s32 x off) (pointer-ref-c-sint32 x off))
(define (ptr-read/u32 x off) (pointer-ref-c-uint32 x off))
(define (ptr-read/s64 x off) (pointer-ref-c-sint64 x off))
(define (ptr-read/u64 x off) (pointer-ref-c-uint64 x off))
(define (ptr-write/s8! x off v) (pointer-set-c-sint8! x off v))
(define (ptr-write/u8! x off v) (pointer-set-c-uint8! x off v))
(define (ptr-write/s16! x off v) (pointer-set-c-sint16! x off v))
(define (ptr-write/u16! x off v) (pointer-set-c-uint16! x off v))
(define (ptr-write/s32! x off v) (pointer-set-c-sint32! x off v))
(define (ptr-write/u32! x off v) (pointer-set-c-uint32! x off v))
(define (ptr-write/s64! x off v) (pointer-set-c-sint64! x off v))
(define (ptr-write/u64! x off v) (pointer-set-c-uint64! x off v))
 
(define-read-asciiz ptr-read/asciiz ptr-read/u8)
(define-write-asciiz ptr-write/asciiz! ptr-write/u8!)                           


;; Guile style bytevector->pointer
(define (bytevector->pointer bv offs)
  ;; FIXME: Do we have to use bytevector->guarded-memory ??
  (call-with-values (lambda () (bytevector->memory bv))
                    (lambda (ptr _) (pointer-add ptr offs))))
         
(define (yuniffi-nccc-call func
                           in in-offset in-size
                           out out-offset out-size)
  (let ((inp (bytevector->pointer in (* 8 in-offset)))
        (outp (bytevector->pointer out (* 8 out-offset))))
    (func inp in-size outp out-size)))

(define nccc-callout-maker
  (make-c-callout-maker 'void '(pointer signed-int pointer signed-int)))

(define (module-load path) (dlopen path))
(define (module-path) (library-source-search-path))

(define yuniffi-module-load (make-simpleloader module-path module-load))
         
(define (yuniffi-module-lookup handle str)
  (nccc-callout-maker (dlsym handle str)))
         
)

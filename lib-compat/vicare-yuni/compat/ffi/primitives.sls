(library (vicare-yuni compat ffi primitives)
         (export yuniffi-nccc-call
                 yuniffi-nccc-ptr->callable
                 yuniffi-nccc-proc-register
                 yuniffi-nccc-proc-release
                 yuniffi-module-load
                 yuniffi-module-lookup
                 yuniffi-callback-helper
 
                 ;; Memory OPs (pointers)
                 ptr?
                 ptr-read/w64ptr
                 ptr-write/w64ptr!
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
                 (only (vicare)
                       bytevector->memory
                       pointer-add
                       )
                 (only (vicare libraries)
                       library-source-search-path)
                 (only (vicare platform constants)
                       SIZEOF_SIZE_T)
                 (only (rnrs) bitwise-and assertion-violation)
                 (yuni compat bitwise primitives)
                 (yuni ffi runtime simpleloader)
                 (yuni ffi runtime simplestrings)
                 (vicare ffi))

(define nccc-callback-maker
  (make-c-callback-maker 'void '(pointer signed-int pointer signed-int)))
(define (yuniffi-callback-helper) #f)
(define (yuniffi-nccc-proc-register proc)
  (nccc-callback-maker proc))
(define (yuniffi-nccc-proc-release proc)
  (free-c-callback proc))
;; A bit different from nmosh
(define (ptr? x) (pointer? x))
 
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

(define filtptrval
  (case SIZEOF_SIZE_T
    ((4)
     (lambda (v) (bitwise-and #xffffffff v)))
    ((8)
     (lambda (v) v))
    (else
      (assertion-violation 'filtptrval "Unknown size_t size!" SIZEOF_SIZE_T))))

(define (ptr-read/w64ptr x off)
  (integer->pointer (filtptrval (ptr-read/u64 x off))))
(define (ptr-write/w64ptr! x off v)
  (ptr-write/u64! x off (pointer->integer (filtptrval v))))
(define (bv-read/w64ptr x off)
  (integer->pointer (filtptrval (bv-read/u64 x off))))
(define (bv-write/w64ptr! x off v)
  (bv-write/u64! x off (pointer->integer (filtptrval v))))
 
(define-read-asciiz ptr-read/asciiz ptr-read/u8)
(define-write-asciiz ptr-write/asciiz! ptr-write/u8!)                           
         
(define (yuniffi-nccc-call func
                           in in-offset in-size
                           out out-offset out-size)
  (unless (and (= 0 in-offset) (= 0 out-offset))
    (error "Not implemented"))
  (func in in-size out out-size))

(define nccc-callout-maker
  (make-c-callout-maker 'void '(pointer signed-int pointer signed-int)))

(define (module-load path) (dlopen path))
(define (module-path) (library-source-search-path))

(define yuniffi-module-load (make-simpleloader module-path module-load))

(define yuniffi-nccc-ptr->callable nccc-callout-maker)
         
(define (yuniffi-module-lookup handle str)
  (nccc-callout-maker (dlsym handle str)))
         
)

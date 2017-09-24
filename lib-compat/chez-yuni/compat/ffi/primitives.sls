(library (chez-yuni compat ffi primitives)
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
                 (only (rnrs)
                       bytevector-u64-native-ref
                       bytevector-u64-native-set!)
                 (only (chezscheme)
                       library-directories
                       foreign-ref
                       foreign-set!
                       foreign-procedure
                       load-shared-object)
                 (yuni ffi runtime simpleloader)
                 (yuni ffi runtime simplestrings))
;; A bit different from nmosh
(define (ptr? x) (integer? x))
 
(define (ptr-read/s8 x off) (foreign-ref 'integer-8 x off))
(define (ptr-read/u8 x off) (foreign-ref 'unsigned-8 x off))
(define (ptr-read/s16 x off) (foreign-ref 'integer-16 x off))
(define (ptr-read/u16 x off) (foreign-ref 'unsigned-16 x off))
(define (ptr-read/s32 x off) (foreign-ref 'integer-32 x off))
(define (ptr-read/u32 x off) (foreign-ref 'unsigned-32 x off))
(define (ptr-read/s64 x off) (foreign-ref 'integer-64 x off))
(define (ptr-read/u64 x off) (foreign-ref 'unsigned-64 x off))
(define (ptr-write/s8! x off v) (foreign-set! 'integer-8 x off v))
(define (ptr-write/u8! x off v) (foreign-set! 'unsigned-8 x off v))
(define (ptr-write/s16! x off v) (foreign-set! 'integer-16 x off v))
(define (ptr-write/u16! x off v) (foreign-set! 'unsigned-16 x off v))
(define (ptr-write/s32! x off v) (foreign-set! 'integer-32 x off v))
(define (ptr-write/u32! x off v) (foreign-set! 'unsigned-32 x off v))
(define (ptr-write/s64! x off v) (foreign-set! 'integer-64 x off v))
(define (ptr-write/u64! x off v) (foreign-set! 'unsigned-64 x off v))

(define (ptr-read/w64ptr x off)
  (ptr-read/u64 x off))

(define (bv-read/w64ptr x off)
  (bytevector-u64-native-ref x off))

(define (bv-write/w64ptr! x off v)
  (bytevector-u64-native-set! x off v))
 
(define-read-asciiz ptr-read/asciiz ptr-read/u8)
(define-write-asciiz ptr-write/asciiz! ptr-write/u8!)                           
         
(define (yuniffi-nccc-call func
                           in in-offset in-size
                           out out-offset out-size)
  (unless (and (= 0 in-offset) (= 0 out-offset))
    (error "Not implemented"))
  (func in in-size out out-size))

(define (module-load path) 
  (and (file-exists? path)
       (begin (load-shared-object path) 
              'xxx-shared-obj-xxx)))

(define (%dirfilt lis) (map car lis))

(define (module-path) (%dirfilt (library-directories)))

(define yuniffi-module-load (make-simpleloader module-path module-load))
         
(define (yuniffi-nccc-ptr->callable ptr)
  (foreign-procedure ptr
                     (u8* int u8* int)
                     void))

(define (yuniffi-module-lookup handle str)
  (foreign-procedure str 
                     (u8* int u8* int)
                     void))
)

(library (chez-yuni compat ffi primitives)
         (export yuniffi-nccc-call
                 yuniffi-nccc-ptr->callable
                 yuniffi-nccc-proc-register
                 yuniffi-nccc-proc-release
                 yuniffi-module-load0
                 yuniffi-module-lookup
                 yuniffi-callback-helper
 
                 ;; Memory OPs (pointers)
                 ptr?
                 ptr-read/s8 ptr-read/u8 ptr-read/s16 ptr-read/u16
                 ptr-read/s32 ptr-read/u32 ptr-read/s64 ptr-read/u64
                 ptr-read/f32 ptr-read/f64
                 ptr-read/ptr ptr-read/sptr ptr-read/uptr
                 ptr-read/asciiz
                 ptr-write/s8! ptr-write/u8! ptr-write/s16! ptr-write/u16!
                 ptr-write/s32! ptr-write/u32! ptr-write/s64! ptr-write/u64!
                 ptr-write/f32! ptr-write/f64!
                 ptr-write/ptr! ptr-write/sptr! ptr-write/uptr!
                 ptr-write/asciiz!

                 bv-read/ptr bv-write/ptr!
                 
                 ;; Buffer primitives
                 buf-alloc
                 buf-free
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
                       foreign-alloc
                       foreign-free
                       load-shared-object
                       lock-object
                       unlock-object
                       foreign-callable
                       foreign-callable-entry-point
                       foreign-callable-code-object)
                 (yuni ffi runtime simplestrings))

         
(define (yuniffi-callback-helper) #f)
(define (yuniffi-nccc-proc-register proc)
  (let ((r (foreign-callable proc (void* void*) void)))
   (lock-object r)
   (foreign-callable-entry-point r)))
(define (yuniffi-nccc-proc-release ptr)
  (unlock-object (foreign-callable-code-object ptr)))

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
(define (ptr-read/f32 x off) (foreign-ref 'single-float x off))
(define (ptr-read/f64 x off) (foreign-ref 'double-float x off))
(define (ptr-read/sptr x off) (foreign-ref 'iptr x off))
(define (ptr-read/uptr x off) (foreign-ref 'uptr x off))
(define (ptr-read/ptr x off) (ptr-read/uptr x off))
(define (ptr-write/s8! x off v) (foreign-set! 'integer-8 x off v))
(define (ptr-write/u8! x off v) (foreign-set! 'unsigned-8 x off v))
(define (ptr-write/s16! x off v) (foreign-set! 'integer-16 x off v))
(define (ptr-write/u16! x off v) (foreign-set! 'unsigned-16 x off v))
(define (ptr-write/s32! x off v) (foreign-set! 'integer-32 x off v))
(define (ptr-write/u32! x off v) (foreign-set! 'unsigned-32 x off v))
(define (ptr-write/s64! x off v) (foreign-set! 'integer-64 x off v))
(define (ptr-write/u64! x off v) (foreign-set! 'unsigned-64 x off v))
(define (ptr-write/f32! x off v) (foreign-set! 'single-float x off v))
(define (ptr-write/f64! x off v) (foreign-set! 'double-float x off v))
(define (ptr-write/sptr! x off v) (foreign-set! 'iptr x off v))
(define (ptr-write/uptr! x off v) (foreign-set! 'uptr x off v))
(define (ptr-write/ptr! x off v) (ptr-write/uptr! x off v))

(define (bv-read/ptr bv off)
  ;; FIXME: Assumes 64bit
  (bytevector-u64-native-ref bv off))
(define (bv-write/ptr! bv off v)
  ;; FIXME: Assumes 64bit
  (bytevector-u64-native-set! bv off v))

(define (buf-alloc siz) (foreign-alloc siz))
(define (buf-free ptr) (foreign-free ptr))
 
(define-read-asciiz ptr-read/asciiz ptr-read/u8)
(define-write-asciiz ptr-write/asciiz! ptr-write/u8!)                           
         
(define (yuniffi-nccc-call func in out) (func in out))

(define (yuniffi-module-load0 path) ;; => handle(bogus)
  (and (file-exists? path)
       (begin (load-shared-object path)
              'xxx-shared-obj-xxx)))
         
(define (yuniffi-nccc-ptr->callable ptr)
  (foreign-procedure ptr (uptr uptr) void))

(define (yuniffi-module-lookup handle str)
  (foreign-procedure str (uptr uptr) void))
)

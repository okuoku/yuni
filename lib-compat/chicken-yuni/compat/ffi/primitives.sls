(library (chicken-yuni compat ffi primitives)
         (export
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

           yuniffi-nccc-call)

         (import
           (yuni scheme)
           (yuni ffi runtime bootstraploader)
           (yuni ffi runtime simpleloader)
           (yuni ffi runtime simplestrings)
           (chicken)
           (yuniffi-chicken)
           (lolevel))

(define (yuniffi-nccc-call func in in-offset in-len out out-offset out-len)
  (%%yuniffi-nccc-call func in in-offset in-len out out-offset out-len))

;; Pointer handlers
(define integer->ptr address->pointer)
(define ptr? pointer?)

(define-syntax defptr-ref
  (syntax-rules ()
    ((_ name base)
     (define (name p off)
       (base (pointer+ p off))))))

(define-syntax defptr-set!
  (syntax-rules ()
    ((_ name base)
     (define (name p off v)
       (let ((px (pointer+ p off)))
        (base px v))))))


(defptr-ref ptr-read/s8 pointer-s8-ref)
(defptr-ref ptr-read/u8 pointer-u8-ref)
(defptr-ref ptr-read/s16 pointer-s16-ref)
(defptr-ref ptr-read/u16 pointer-u16-ref)
(defptr-ref ptr-read/s32 pointer-s32-ref)
(defptr-ref ptr-read/u32 pointer-u32-ref)

(defptr-set! ptr-write/s8! pointer-s8-set!)
(defptr-set! ptr-write/u8! pointer-u8-set!)
(defptr-set! ptr-write/s16! pointer-s16-set!)
(defptr-set! ptr-write/u16! pointer-u16-set!)
(defptr-set! ptr-write/s32! pointer-s32-set!)
(defptr-set! ptr-write/u32! pointer-u32-set!)

(define-read-asciiz ptr-read/asciiz ptr-read/u8)
(define-write-asciiz ptr-write/asciiz! ptr-write/u8!)

(define (ptr-read/s64 p off)
  (let ((lo (ptr-read/s32 p off))
        (hi (ptr-read/s32 p (+ 4 off))))
    (bitwise-ior (arithmetic-shift hi 32)
                lo)))
(define (ptr-read/u64 p off)
  (let ((lo (ptr-read/s32 p off))
        (hi (ptr-read/s32 p (+ 4 off))))
    (+ (arithmetic-shift hi 32)
       lo)))

(define (ptr-write/s64! p off v)
  (let ((lo (bitwise-and #xffffffff v))
        (hi (bitwise-and #xffffffff
                         (arithmetic-shift v -32))))
    (ptr-write/u32! p off lo)
    (ptr-write/u32! p (+ 4 off) hi)))

(define ptr-write/u64! ptr-write/s64!)

;; Bootstrap loader
(define-values
  (dlopen dlsym)
  (make-bootstraploader yuniffi-nccc-call
                        (lambda () %%yuniffi-nccc-bootstrap)
                        ptr-write/asciiz!
                        integer->ptr))

(define (%module-path) (list (%%yuniffi-module-prefix)))

(define (%module-load path) (dlopen path))

(define (yuniffi-module-lookup handle str) (dlsym handle str))
(define yuniffi-module-load (make-simpleloader %module-path %module-load))
)

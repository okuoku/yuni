(library (picrin-yuni compat ffi primitives)
         (export
           yuniffi-module-load
           yuniffi-module-lookup

           ;; Memory OPs (pointers)
           ptr? 
           ptr-read/w64ptr
           ptr-read/s8 ptr-read/u8 ptr-read/s16 ptr-read/u16
           ;ptr-read/s32 ptr-read/u32 ptr-read/s64 ptr-read/u64
           ptr-read/asciiz
           ptr-write/s8! ptr-write/u8! ptr-write/s16! ptr-write/u16!
           ;ptr-write/s32! ptr-write/u32! ptr-write/s64! ptr-write/u64!
           ptr-write/asciiz!

           bv-read/w64ptr
           bv-write/w64ptr!

           yuniffi-nccc-call)

         (import
           (yuni scheme)
           (yuni ffi runtime bootstraploader)
           (yuni ffi runtime simpleloader)
           (yuni ffi runtime simplestrings)
           (srfi 60)
           (yuniffi-config)
           (yuniffi-picrin))

(define (yuniffi-nccc-call func in in-offset in-len out out-offset out-len)
  (%%yuniffi_nccc_call func in in-offset in-len out out-offset out-len))

;; Pointer handlers
(define (ptr? x) 
  (%%yunipointer? x))

(define-syntax defptr-ref
  (syntax-rules ()
    ((_ name base siz)
     (define (name p off)
       (base p off siz)))))

(define-syntax defptr-set!
  (syntax-rules ()
    ((_ name base siz)
     (define (name p off v)
       (base p off siz v)))))

(defptr-ref ptr-read/s8 %%yunipointer_fetch_signed 1)
(defptr-ref ptr-read/u8 %%yunipointer_fetch_unsigned 1)
(defptr-ref ptr-read/s16 %%yunipointer_fetch_signed 2)
(defptr-ref ptr-read/u16 %%yunipointer_fetch_unsigned 2)
(defptr-ref ptr-read/w64ptr %%yunipointer_fetchpointer_unsigned 0)
(defptr-set! ptr-write/s8! %%yunipointer_store_signed 1)
(defptr-set! ptr-write/u8! %%yunipointer_store_unsigned 1)
(defptr-set! ptr-write/s16! %%yunipointer_store_signed 2)
(defptr-set! ptr-write/u16! %%yunipointer_store_unsigned 2)

(define-read-asciiz ptr-read/asciiz ptr-read/u8)
(define-write-asciiz ptr-write/asciiz! ptr-write/u8!)

(define (bv-read/w64ptr bv off)
  (let ((ptr (%%yunipointer_frombytevector bv 0)))
   (%%yunipointer_fetchpointer_unsigned ptr off 0)))

(define (bv-write/w64ptr! bv off v)
  (let ((ptr (%%yunipointer_frombytevector bv 0)))
   (%%yunipointer_storepointer_unsigned ptr off 0 v)))

;; Bootstrap loader
(define-values
  (dlopen dlsym)
  (make-bootstraploader yuniffi-nccc-call
                        %%yuniffi_nccc_bootstrap
                        ptr?
                        bv-read/w64ptr
                        bv-write/w64ptr!
                        ptr-write/asciiz!))

(define (%module-path) (list (%%yuniffi-module-prefix)))

(define (%module-load path) 
  (dlopen path))

(define (yuniffi-module-lookup handle str) (dlsym handle str))
(define yuniffi-module-load (make-simpleloader %module-path %module-load))
)

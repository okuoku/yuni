(library (gauche-yuni compat ffi primitives)
         (export
           yuniffi-nccc-call
           yuniffi-nccc-ptr->callable
           yuniffi-nccc-proc-register ;; reexport
           yuniffi-nccc-proc-release ;; reexport
           yuniffi-module-load
           yuniffi-module-lookup
           yuniffi-callback-helper
           yuniffi-module-path

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
           bv-write/w64ptr!)
         (import (yuni scheme)
                 (yuni-runtime gauche loadpath)
                 (yuni ffi runtime bootstraploader)
                 (yuni ffi runtime simpleloader)
                 (yuni ffi runtime simplestrings)
                 (rename (yuniffi)
                         (yuniffi-nccc-call yuniffi-nccc-call/raw)
                         ) ;; see yuni/yunistub/gauche
                 )

;; Callback ops

(define (yuniffi-callback-helper) (yuniffi-nccc-get-callback-bridge))

;; Memory ops

(define (ptr? x) (yuniptr? x))
(define (integer->ptr x) (yuniffi-pointer-fromint x))

(define-syntax defread
  (syntax-rules ()
    ((_ nam proc width)
     (define (nam p off)
       (proc p off width)))))

(defread ptr-read/s8 yuniffi-pointer-fetch-signed 1)
(defread ptr-read/s16 yuniffi-pointer-fetch-signed 2)
(defread ptr-read/s32 yuniffi-pointer-fetch-signed 4)
(defread ptr-read/s64 yuniffi-pointer-fetch-signed 8)

(defread ptr-read/u8 yuniffi-pointer-fetch-unsigned 1)
(defread ptr-read/u16 yuniffi-pointer-fetch-unsigned 2)
(defread ptr-read/u32 yuniffi-pointer-fetch-unsigned 4)
(defread ptr-read/u64 yuniffi-pointer-fetch-unsigned 8)

(define-syntax defwrite
  (syntax-rules ()
    ((_ nam proc width)
     (define (nam p off v)
       (proc p off width v)))))

(defwrite ptr-write/s8! yuniffi-pointer-store 1)
(defwrite ptr-write/s16! yuniffi-pointer-store 2)
(defwrite ptr-write/s32! yuniffi-pointer-store 4)
(defwrite ptr-write/s64! yuniffi-pointer-store 8)
         
(defwrite ptr-write/u8! yuniffi-pointer-store 1)
(defwrite ptr-write/u16! yuniffi-pointer-store 2)
(defwrite ptr-write/u32! yuniffi-pointer-store 4)
(defwrite ptr-write/u64! yuniffi-pointer-store 8)

(define-read-asciiz ptr-read/asciiz ptr-read/u8)
(define-write-asciiz ptr-write/asciiz! ptr-write/u8!)

(define ptr-read/w64ptr yuniffi-pointer-fetch-p64)
(define ptr-write/w64ptr! yuniffi-pointer-store-p64)
(define bv-read/w64ptr yuniffi-pointer-fetch-p64/bv)
(define bv-write/w64ptr! yuniffi-pointer-store-p64/bv)
         
(define (yuniffi-nccc-call func
                           in in-offset in-size
                           out out-offset out-size)
  (yuniffi-nccc-call/raw func in in-offset in-size out out-offset out-size))

(define (yuniffi-nccc-ptr->callable ptr) ptr)

(define-values (dlopen dlsym)
               (make-bootstraploader yuniffi-nccc-call
                                     yuniffi-nccc-bootstrap
                                     ptr?
                                     bv-read/w64ptr
                                     bv-write/w64ptr!
                                     ptr-write/asciiz!))

(define (yuniffi-module-lookup handle str) (dlsym handle str))

;; Hook points
(define (module-path) (loadpath))
(define (module-load path) (dlopen path))

(define yuniffi-module-load (make-simpleloader module-path module-load))
)

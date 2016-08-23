(library (gambit-yuni compat ffi primitives-phase0)
         (export
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

           ;; For phase1
           ensure-yuniffi-loaded!
           )
         (import (yuni scheme)
                 (yuni ffi runtime simplestrings)
                 (yuni compat bitwise primitives))

(define-primitive-names/yunifake
  %%yuniffi-gambit-modpath
  load
  %%yuniffi-nccc-call
  %%yuniffi-nccc-bootstrap
  %%yuniffi-fetch-s8
  %%yuniffi-fetch-s16
  %%yuniffi-fetch-s32
  %%yuniffi-fetch-s64
  %%yuniffi-fetch-u8
  %%yuniffi-fetch-u16
  %%yuniffi-fetch-u32
  %%yuniffi-fetch-u64
  %%yuniffi-store-s8
  %%yuniffi-store-s16
  %%yuniffi-store-s32
  %%yuniffi-store-s64
  %%yuniffi-store-u8
  %%yuniffi-store-u16
  %%yuniffi-store-u32
  %%yuniffi-store-u64)

(define ptr-read/s8 %%yuniffi-fetch-s8)
(define ptr-read/s16 %%yuniffi-fetch-s16)
(define ptr-read/s32 %%yuniffi-fetch-s32)
(define ptr-read/s64 %%yuniffi-fetch-s64)
(define ptr-read/u8 %%yuniffi-fetch-u8)
(define ptr-read/u16 %%yuniffi-fetch-u16)
(define ptr-read/u32 %%yuniffi-fetch-u32)
(define ptr-read/u64 %%yuniffi-fetch-u64)
(define ptr-write/s8! %%yuniffi-store-s8)
(define ptr-write/s16! %%yuniffi-store-s16)
(define ptr-write/s32! %%yuniffi-store-s32)
(define ptr-write/s64! %%yuniffi-store-s64)
(define ptr-write/u8! %%yuniffi-store-u8)
(define ptr-write/u16! %%yuniffi-store-u16)
(define ptr-write/u32! %%yuniffi-store-u32)
(define ptr-write/u64! %%yuniffi-store-u64)

(define (ptr-read/w64ptr x off)
  (ptr-read/u64 x off))
(define (ptr-write/w64ptr! x off v)
  (ptr-write/u64! x off v))
(define (bv-read/w64ptr x off)
  (bv-read/u64 x off))
(define (bv-write/w64ptr! x off v)
  (bv-write/u64! x off v))

;; FIXME: Not needed
(define (search-ffi-module) ;; => path without .o1
  (let loop ((libs (list %%yuniffi-gambit-modpath)))
   (if (pair? libs)
     (let ((pth (car libs))
           (next (cdr libs)))
       (let* ((base (string-append pth "/" "yuniffi-gambit"))
              (file (string-append base ".o1")))
         (display (list 'CHECK: file)) (newline)
         (cond
           ((file-exists? file) base)
           (else (loop next)))))
     #f)))

(define yuniffi-module-loaded #t)

;; FIXME: Not needed??
(define (ensure-yuniffi-loaded!)
  (unless yuniffi-module-loaded
    (let ((pth (search-ffi-module)))
     (display (list 'FFI-MOD pth)) (newline)
     (when pth
       (core-load pth)
       (set! yuniffi-module-loaded #t))))) 

(define (ptr? x) (integer? x))

(define-read-asciiz ptr-read/asciiz ptr-read/u8)
(define-write-asciiz ptr-write/asciiz! ptr-write/u8!)

)

(library (rapid-gambit-yuni compat ffi primitives)
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
                 (yuni ffi runtime bootstraploader)
                 (yuni ffi runtime simpleloader)
                 (yuni ffi runtime simplestrings)
                 
                 (rapid primitive))

(define-primitive libpath '%%rapid-gambit-libpath)
(define-primitive core-load 'load)
(define-primitive %yuniffi-nccc-call '%%yuniffi-nccc-call)
(define-primitive %yuniffi-nccc-bootstrap '%%yuniffi-nccc-bootstrap)
(define-primitive ptr-read/s8 '%%yuniffi-fetch-s8)
(define-primitive ptr-read/s16 '%%yuniffi-fetch-s16)
(define-primitive ptr-read/s32 '%%yuniffi-fetch-s32)
(define-primitive ptr-read/s64 '%%yuniffi-fetch-s64)
(define-primitive ptr-read/u8 '%%yuniffi-fetch-u8)
(define-primitive ptr-read/u16 '%%yuniffi-fetch-u16)
(define-primitive ptr-read/u32 '%%yuniffi-fetch-u32)
(define-primitive ptr-read/u64 '%%yuniffi-fetch-u64)
(define-primitive ptr-write/s8! '%%yuniffi-store-s8)
(define-primitive ptr-write/s16! '%%yuniffi-store-s16)
(define-primitive ptr-write/s32! '%%yuniffi-store-s32)
(define-primitive ptr-write/s64! '%%yuniffi-store-s64)
(define-primitive ptr-write/u8! '%%yuniffi-store-u8)
(define-primitive ptr-write/u16! '%%yuniffi-store-u16)
(define-primitive ptr-write/u32! '%%yuniffi-store-u32)
(define-primitive ptr-write/u64! '%%yuniffi-store-u64)

(define (search-ffi-module) ;; => path without .o1
  (let loop ((libs (libpath)))
   (if (pair? libs)
     (let ((pth (car libs))
           (next (cdr libs)))
       (let* ((base (string-append pth "/" "yuniffi-gambit"))
              (file (string-append base ".o1")))
         ;(display (list 'CHECK: file)) (newline)
         (cond
           ((file-exists? file) base)
           (else (loop next)))))
     #f)))

(define yuniffi-module-loaded #f)

(define (ensure-yuniffi-loaded!)
  (unless yuniffi-module-loaded
    (let ((pth (search-ffi-module)))
     ;(display (list 'FFI-MOD pth)) (newline)
     (when pth
       (core-load pth)
       (set! yuniffi-module-loaded #t))))) 


(define (ptr? x) (integer? x))
(define (integer->ptr x) x)

(define-read-asciiz ptr-read/asciiz ptr-read/u8)
(define-write-asciiz ptr-write/asciiz! ptr-write/u8!)

(define-values (dlopen dlsym)
               (make-bootstraploader yuniffi-nccc-call
                                     yuniffi-nccc-bootstrap
                                     ptr-write/asciiz!
                                     integer->ptr))

(define (yuniffi-nccc-call . args)
  (ensure-yuniffi-loaded!)
  (apply %yuniffi-nccc-call args))

(define (yuniffi-nccc-bootstrap)
  (ensure-yuniffi-loaded!)
  (%yuniffi-nccc-bootstrap))

(define (%module-load path) (dlopen path))
(define (%module-path) (libpath))
(define yuniffi-module-load (make-simpleloader %module-path %module-load))
(define (yuniffi-module-lookup handle str) (dlsym handle str))

)

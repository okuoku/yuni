(library (yuni ffi runtime yunistub-abiv0)
         (export
           define-library-state
           ;; For -constants library
           define-constant
           define-layout-constant
           define-aggregate-entry-constant
           define-bridgestub-constant
           )
         (import (yuni scheme)
                 (yuni base match)
                 (yuni ffi abi abiv0-runtime)
                 (yuni compat hashtables)
                 (yuni compat bitwise primitives)
                 (yuni compat ffi primitives))

(define-syntax define-library-state
  (syntax-rules ()
    ((_ name dllname cname)
     (define name (vector #f dllname cname (make-entries-ht))))))

(define (library-state-initialized? v) (vector-ref v 0))
(define (library-state-set-initialized! v b) (vector-set! v 0 b))
(define (library-state-dllname v) (vector-ref v 1))
(define (library-state-cname v) (vector-ref v 2))
(define (library-state-ht v) (vector-ref v 3))

(define (make-entries-ht) (make-hashtable string-hash string=?))
(define (register-entries! libstate lis)
  (define ht (library-state-ht libstate))
  (define (itr e)
    (let ((name (car e))
          (dat (cdr e)))
      (hashtable-set! ht name dat)))
  (when (pair? lis)
    (for-each itr lis)))

(define (ensure-library-initialized libstate)
  (unless (library-state-initialized? libstate)
    ;; Create a hashtable
    (let ((dll (yuniffi-module-load (library-state-dllname libstate)))
          (cname (library-state-cname libstate)))
      ;(display (list 'CNAME: cname 'DLL: dll))(newline)
      (when dll
        (let ((const (yuniffi-abiv0-lookup/constants dll cname))
              (funcs (yuniffi-abiv0-lookup/bridgestubs dll cname)))
          (register-entries! libstate 
                             (yuniffi-abiv0-get-table const))
          (register-entries! libstate 
                             (yuniffi-abiv0-get-table funcs)))))
    (library-state-set-initialized! libstate #t)))

(define (realize-aggregate-entry-constant libstate dbname)
  (define ht (library-state-ht libstate))
  (let ((obj (hashtable-ref ht dbname #f)))
   (and obj
        (match obj
               ((flags value size offset)
                (cons size offset))))))

(define (realize-layout-constant libstate dbname)
  (define ht (library-state-ht libstate))
  (let ((obj (hashtable-ref ht dbname #f)))
   (and obj
        (match obj
               ((flags value size offset)
                size)))))

(define (realize-bridgestub-constant libstate dbname)
  (define ht (library-state-ht libstate))
  (let ((obj (hashtable-ref ht dbname #f)))
   (and obj
        (match obj
               ((flags value size offset)
                (integer->ptr value))))))

(define (realize-constant libstate typesym dbname)
  (define ht (library-state-ht libstate))
  (let ((obj (hashtable-ref ht dbname #f)))
   (and obj
        (match obj
               ((flags value size offset)
                ;; Ignore size for now
                (case typesym
                  ((unsigned)
                   value)
                  ((pointer)
                   (integer->ptr value))
                  ((signed)
                   (let ((bv (make-bytevector 8)))
                    (bv-write/u64! bv 0 value)
                    (bv-read/s64 bv 0)))
                  ((real)
                   (let ((bv (make-bytevector 8)))
                    (bv-write/u64! bv 0 value)
                    (bv-read/f64 bv 0)) 
;                   #| ;; FIXME: Support real size formatting...
;                   (case size
;                     ((4) 
;                      (let ((bv (make-bytevector 4)))
;                       (bv-write/u32! bv 0 value)
;                       (bv-read/f32 bv 0)))
;                     ((8) 
;                      (let ((bv (make-bytevector 8)))
;                       (bv-write/u64! bv 0 value)
;                       (bv-read/f64 bv 0)))
;                     (else
;                       (error "Unknown real format" size)))
;                   |#
                   )
                  (else
                    (error "Unsupported type" typesym))))))))

(define-syntax define-constant
  (syntax-rules ()
    ((_ name libstate dbname type)
     (define name
       (begin
         (ensure-library-initialized libstate)
         (realize-constant libstate 'type dbname))))))

(define-syntax define-aggregate-entry-constant
  (syntax-rules ()
    ((_ name libstate dbname)
     (define name 
       (begin 
         (ensure-library-initialized libstate)
         (realize-aggregate-entry-constant libstate dbname))))))

(define-syntax define-layout-constant
  (syntax-rules ()
    ((_ name libstate dbname)
     (define name 
       (begin
         (ensure-library-initialized libstate)
         (realize-layout-constant libstate dbname))))))

(define-syntax define-bridgestub-constant
  (syntax-rules ()
    ((_ name libstate (stubtype dbname) ...)
     (define name
       (begin
         (ensure-library-initialized libstate)
         (list 
           (cons 'stubtype (realize-bridgestub-constant libstate dbname))
           ...))))))

)

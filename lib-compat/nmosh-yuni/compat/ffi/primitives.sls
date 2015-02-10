(library (nmosh-yuni compat ffi primitives)
         (export yuniffi-nccc-call
                 yuniffi-module-load
                 yuniffi-module-lookup
                 )
         (import (yuni scheme)
                 (only (mosh ffi)
                       lookup-shared-library ;; (%ffi-lookup handle sym)
                       close-shared-library  ;; (%ffi-close handle)
                       shared-library-error  ;; (%ffi-error)
                       open-shared-library   ;; (%ffi-open path)
                       )
                 (nmosh global-flags))

;; 

(define %nmosh-yuniffi-call-nccc
  (let ((f (get-global-flag '%nmosh-yuniffi-call-nccc)))
   (if f f (lambda e (error "yuniFFI support is not compiled-in!"))))) 

(define *empty* (make-bytevector 0))

(define (yuniffi-nccc-call func 
                           in in-offset in-size
                           out out-offset out-size)
  (define xin (or in *empty*))
  (define xout (or out *empty*))
  (when (and (not in) (not (= in-size 0)))
    (error "in-size is not zero" in-size))
  (when (and (not out) (not (= out-size 0)))
    (error "out-size is not zero" out-size))

  (%nmosh-yuniffi-call-nccc
    func
    xin in-offset in-size
    xout out-offset out-size))

(define (yuniffi-module-load path) ;; => handle/#f
  (guard (c ((#t #f)))
         (open-shared-library path)))

(define (yuniffi-module-lookup handle str)
  (lookup-shared-library handle (string->symbol str)))

)

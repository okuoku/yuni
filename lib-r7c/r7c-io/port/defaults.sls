(library (r7c-io port defaults)
         (export
           current-input-port
           current-output-port
           current-error-port
           with-input-from-file
           with-output-to-file)
         (import (r7c-basic syntax define)
                 (r7c-system core)
                 (r7c heap fixnum)
                 (r7c syntax and)
                 (r7c-io port files))

(define param-current-input-port (open-input-file 'stdin))
(define param-current-output-port (open-output-file 'stdout))
(define param-current-error-port (open-output-file 'stderr))

(define (current-input-port) param-current-input-port)
(define (current-output-port) param-current-output-port)
(define (current-error-port) param-current-error-port)

;; NB: No support for escape.
(define (with-input-from-file fn proc)
  (let ((ip param-current-input-port))
   (set! param-current-input-port (open-input-file fn))
   (call-with-values proc 
                     (lambda vals
                       (set! param-current-input-port ip)
                       (apply values vals)))))

;; NB: No support for escape.
(define (with-output-to-file fn proc)
  (let ((op param-current-output-port))
   (set! param-current-output-port (open-output-file fn))
   (call-with-values proc
                     (lambda vals
                       (set! param-current-output-port op)
                       (apply values vals)))))
         
)

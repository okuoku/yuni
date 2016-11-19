(library (r7b-util s45)
         (export lazy eager delay force promise?)
         (import (rnrs) (rnrs mutable-pairs))

;; r7b-util addition for R7RS draft 7
(define promise? list?)

;; From R7RS

(define (%make-promise done? proc)
  (list (cons done? proc)))

(define (%promise-done? x)
  (caar x))
(define (%promise-value x)
  (cdar x))
(define (%promise-update! new old)
  (set-car! (car old) (%promise-done? new))
  (set-cdr! (car old) (%promise-value new))
  (set-car! new (car old)))


;; Renamed to SRFI-45 names
(define-syntax lazy ;; delay-force
  (syntax-rules ()
    ((_ expr)
     (%make-promise #t (lambda () expr)))))

(define-syntax delay
  (syntax-rules ()
    ((_ expr)
     (lazy (%make-promise #t expr)))))

(define (force promise)
  (if (%promise-done? promise)
    (%promise-value promise)
    (let ((promise* ((%promise-value promise))))
     (unless (%promise-done? promise)
       (%promise-update! promise* promise))
     (force promise))))

;; wrapped %make-promise
(define (eager val)
  (let ((v val))
   (%make-promise #t (lambda () v))))


)

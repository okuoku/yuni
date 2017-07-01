(library (r7c-numeric std inexact)
         (export acos asin atan cos
                 exp finite? infinite?
                 log nan? sin sqrt tan)
         (import (r7c-basic syntax define)
                 (r7c-system core)
                 (r7c syntax cond)
                 (r7c syntax and)
                 (r7c heap pair)
                 (r7c heap fixnum)
                 (r7c heap flonum))


(define (%inx a)
  ;; FIXME: It seems there is no way to do this with coreops
  ;; (inexact a)
  (if (integer? a) ($fx->fl a) a))

(define finite? $fl-finite?)
(define infinite? $fl-infinite?)
(define nan? $fl-nan?)
         
(define (acos x) ($fl-acos (%inx x)))
(define (asin x) ($fl-asin (%inx x)))
(define (atan y . x?) 
  (if (null? x?)
    ($fl-atan (%inx y))
    ($fl-atan2 (%inx y) (%inx (car x?)))))
(define (cos x) ($fl-cos (%inx x)))
(define (tan x) ($fl-tan (%inx x)))
(define (exp x) ($fl-exp (%inx x)))
(define (log a . b?)
  (if (null? b?)
    ($fl-loge (%inx a))
    ($fl-log (%inx a) (%inx (car b?)))))
(define (sin x) ($fl-sin (%inx x)))
(define (sqrt x) ($fl-sqrt (%inx x)))

         
)

(library (r7c-lib cxr)
         (export
           caaar caadr cadar caddr cdaar cdadr cddar cdddr
           caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr 
           cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr )
         (import (r7c-core syntax)
                 (r7c-core runtime))

;; 3
(define (caaar pair)
  (car (caar pair)))
(define (caadr pair)
  (car (cadr pair)))
(define (cadar pair)
  (car (cdar pair)))
(define (caddr pair)
  (car (cddr pair)))
(define (cdaar pair)
  (cdr (caar pair)))
(define (cdadr pair)
  (cdr (cadr pair)))
(define (cddar pair)
  (cdr (cdar pair)))
(define (cdddr pair)
  (cdr (cddr pair)))

;; 4a
(define (caaaar pair)
  (caar (caar pair)))
(define (caaadr pair)
  (caar (cadr pair)))
(define (caadar pair)
  (caar (cdar pair)))
(define (caaddr pair)
  (caar (cddr pair)))
(define (cadaar pair)
  (cadr (caar pair)))
(define (cadadr pair)
  (cadr (cadr pair)))
(define (caddar pair)
  (cadr (cdar pair)))
(define (cadddr pair)
  (cadr (cddr pair)))

;; 4d
(define (cdaaar pair)
  (cdar (caar pair)))
(define (cdaadr pair)
  (cdar (cadr pair)))
(define (cdadar pair)
  (cdar (cdar pair)))
(define (cdaddr pair)
  (cdar (cddr pair)))
(define (cddaar pair)
  (cddr (caar pair)))
(define (cddadr pair)
  (cddr (cadr pair)))
(define (cdddar pair)
  (cddr (cdar pair)))
(define (cddddr pair)
  (cddr (cddr pair)))

)

(library (yuni base dispatch)
         (export dispatch-lambda)
         (import (yuni scheme))

;;

(define-syntax %let-terms
  (syntax-rules ()
    ((_ (var . varrest) (nam . namrest) body)
     (let ((nam var))
      (%let-terms varrest namrest body)))
    ((_ () () body)
     body)
    ((_ var nam body)
     (let ((nam var))
      body))))

(define-syntax %gen-dispatch+case-lambda
  (syntax-rules ()
    ;; Symdispatch-only
    ((_ (first . rest) (((withsymtop . withsymrest) body) ...) ())
     (case first
       ((withsymtop)
        (%let-terms rest withsymrest body))
       ...
       (else
         (error "unknown command" first (list 'withsymtop ...))) ))
    ;; Nodispatch-only
    ((_ f () ((f2 body)))
     ;; NB: only one no-dispatch allowed
     (%let-terms f f2 body))
    ;; Both
    ((_ (first . rest) (((withsymtop . withsymrest) body) ...) 
        ((f elsebody)))
     (case first
       ((withsymtop)
        (%let-terms rest withsymrest body))
       ...
       (else
         (%let-terms (first . rest) f elsebody))))))

(define-syntax %sort-dispatch-clauses
  (syntax-rules (quote)
    ;; Terminate
    ((_ with without f ())
     (%gen-dispatch+case-lambda
       f
       with
       without))
    ;; With
    ((_ with without f ((('sym . rest) body) . next))
     (%sort-dispatch-clauses
       (((sym . rest) body) . with)
       without
       f
       next))
    ;; Without
    ((_ with without f (cur . next))
     (%sort-dispatch-clauses
       with
       (cur . without)
       f
       next))))

(define-syntax %output-case-lambda
  ;; Filter-out null entries and emit case-lambda with formals
  (syntax-rules ()
    ((_ cur (f ()) next ...)
     (%output-case-lambda cur next ...))
    ((_ cur valid-entry next ...)
     (%output-case-lambda (valid-entry . cur) next ...))
    ((_ ((f clauses) ...))
     (case-lambda 
       (f (%sort-dispatch-clauses () () f clauses))
       ...))))

(define-syntax %gen-case-lambda
  (syntax-rules ()
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+)
     (%output-case-lambda ()
                          ;; (case-lambda-formal (body ...))
                          (() $0)
                          ((x1) $1)
                          ((x1 x2) $2)
                          ((x1 x2 x3) $3)
                          ((x1 x2 x3 x4) $4)
                          ((x1 x2 x3 x4 x5) $5)
                          ((x1 x2 x3 x4 x5 x6) $6)
                          ((x1 x2 x3 x4 x5 x6 x7) $7)
                          ((x1 x2 x3 x4 x5 x6 x7 x8) $8)
                          ((x1 x2 x3 x4 x5 x6 x7 x8 x9) $9)
                          ((x1 x2 x3 x4 x5 x6 x7 x8 x9 . x) $9+)
                          ((x1 x2 x3 x4 x5 x6 x7 x8 . x) $8+)
                          ((x1 x2 x3 x4 x5 x6 x7 . x) $7+)
                          ((x1 x2 x3 x4 x5 x6 . x) $6+)
                          ((x1 x2 x3 x4 x5 . x) $5+)
                          ((x1 x2 x3 x4 . x) $4+)
                          ((x1 x2 x3 . x) $3+)
                          ((x1 x2 . x) $2+)
                          ((x1 . x) $1+)
                          (wam $0+)))))

(define-syntax %sort-clauses
  (syntax-rules ()
    ;; Err
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        (((x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 ...) body ...) . bogus))
     (syntax-error "Too many arguments"))
    ;; Terminate
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        ())
     (%gen-case-lambda
       $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+))
    ;; 0
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        ((() body ...) . next))
     (%sort-clauses 
       ((() (begin body ...)) . $0) 
       $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
       next))
    ;; 1
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        (((x1) body ...) . next))
     (%sort-clauses 
       $0 $0+ 
       (((x1) (begin body ...)) . $1) 
       $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
       next))
    ;; 2
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        (((x1 x2) body ...) . next))
     (%sort-clauses 
       $0 $0+ $1 $1+ 
       (((x1 x2) (begin body ...)) . $2) 
       $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
       next))
    ;; 3
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        (((x1 x2 x3) body ...) . next))
     (%sort-clauses 
       $0 $0+ $1 $1+ $2 $2+ 
       (((x1 x2 x3) (begin body ...)) . $3) 
       $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
       next))
    ;; 4
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        (((x1 x2 x3 x4) body ...) . next))
     (%sort-clauses 
       $0 $0+ $1 $1+ $2 $2+ $3 $3+ 
       (((x1 x2 x3 x4) (begin body ...)) . $4)
       $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
       next))
    ;; 5
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        (((x1 x2 x3 x4 x5) body ...) . next))
     (%sort-clauses 
       $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ 
       (((x1 x2 x3 x4 x5) (begin body ...)) . $5) 
       $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
       next))
    ;; 6
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        (((x1 x2 x3 x4 x5 x6) body ...) . next))
     (%sort-clauses 
       $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ 
       (((x1 x2 x3 x4 x5 x6) (begin body ...)) . $6) 
       $6+ $7 $7+ $8 $8+ $9 $9+
       next))
    ;; 7
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        (((x1 x2 x3 x4 x5 x6 x7) body ...) . next))
     (%sort-clauses 
       $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ 
       (((x1 x2 x3 x4 x5 x6 x7) (begin body ...)) . $7) 
       $7+ $8 $8+ $9 $9+
       next))
    ;; 8
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        (((x1 x2 x3 x4 x5 x6 x7 x8) body ...) . next))
     (%sort-clauses 
       $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ 
       (((x1 x2 x3 x4 x5 x6 x7 x8) (begin body ...)) . $8) 
       $8+ $9 $9+
       next))
    ;; 9
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        (((x1 x2 x3 x4 x5 x6 x7 x8 x9) body ...) . next))
     (%sort-clauses 
       $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ 
       (((x1 x2 x3 x4 x5 x6 x7 x8 x9) (begin body ...)) . $9) 
       $9+
       next))
    ;; 9+
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        (((x1 x2 x3 x4 x5 x6 x7 x8 x9 . x) body ...) . next))
     (%sort-clauses 
       $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 
       (((x1 x2 x3 x4 x5 x6 x7 x8 x9 . x) (begin body ...)) . $9+)
       next))
    ;; 8+
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        (((x1 x2 x3 x4 x5 x6 x7 x8 . x) body ...) . next))
     (%sort-clauses 
       $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 
       (((x1 x2 x3 x4 x5 x6 x7 x8 . x) (begin body ...)) . $8+)
       $9 $9+
       next))
    ;; 7+
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        (((x1 x2 x3 x4 x5 x6 x7 . x) body ...) . next))
     (%sort-clauses 
       $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 
       (((x1 x2 x3 x4 x5 x6 x7 . x) (begin body ...)) . $7+)
       $8 $8+ $9 $9+
       next))
    ;; 6+
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        (((x1 x2 x3 x4 x5 x6 . x) body ...) . next))
     (%sort-clauses 
       $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 
       (((x1 x2 x3 x4 x5 x6 . x) (begin body ...)) . $6+)
       $7 $7+ $8 $8+ $9 $9+
       next))
    ;; 5+
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        (((x1 x2 x3 x4 x5 . x) body ...) . next))
     (%sort-clauses 
       $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 
       (((x1 x2 x3 x4 x5 . x) (begin body ...)) . $5+)
       $6 $6+ $7 $7+ $8 $8+ $9 $9+
       next))
    ;; 4+
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        (((x1 x2 x3 x4 . x) body ...) . next))
     (%sort-clauses 
       $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 
       (((x1 x2 x3 x4 . x) (begin body ...)) . $4+)
       $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
       next))
    ;; 3+
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        (((x1 x2 x3 . x) body ...) . next))
     (%sort-clauses 
       $0 $0+ $1 $1+ $2 $2+ $3 
       (((x1 x2 x3 . x) (begin body ...)) . $3+)
       $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
       next))
    ;; 2+
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        (((x1 x2 . x) body ...) . next))
     (%sort-clauses 
       $0 $0+ $1 $1+ $2 
       (((x1 x2 . x) (begin body ...)) . $2+)
       $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
       next))
    ;; 1+
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        (((x1 . x) body ...) . next))
     (%sort-clauses 
       $0 $0+ $1 
       (((x1 . x) (begin body ...)) . $1+)
       $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
       next))
    ;; 0+
    ((_ $0 $0+ $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
        ((x body ...) . next))
     (%sort-clauses 
       $0 
       ((x (begin body ...)) . $0+)
       $1 $1+ $2 $2+ $3 $3+ $4 $4+ $5 $5+ $6 $6+ $7 $7+ $8 $8+ $9 $9+
       next))))

;;; Entry point
(define-syntax dispatch-lambda
  (syntax-rules ()
    ((_ clauses ...)
     (%sort-clauses
       ;; 0 + More
       () ()
       ;; 1
       () ()
       ;; 2
       () ()
       ;; 3
       () ()
       ;; 4
       () ()
       ;; 5
       () ()
       ;; 6
       () ()
       ;; 7
       () ()
       ;; 8
       () ()
       ;; 9
       () ()
       ;; Input
       (clauses ...))))) 
)

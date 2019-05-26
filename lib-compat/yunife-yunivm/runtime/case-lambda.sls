(library (yunife-yunivm runtime case-lambda)
         (export case-lambda)
         (import (yunivm-core-syntax)
                 (yunife-yunivm runtime let))


(define-syntax case-lambda
  (syntax-rules ()
    ;; Short-cut
    ((_ (frm code ...))
     (lambda frm code ...))
    ((_ clauses ...)
     (%%gen-case-lambda-dispatch
       ()
       ()
       ()
       clauses ...))))
 
(define-syntax %%emit-case-lambda-body
  (syntax-rules ()
    ((_ () () () count x)
     (error "case-lambda: unmatched" count))
    ((_ (n ...) (#t ...) ((frm body ...)) count x)
     (let ((frm x))
      body ...))
    ((_ (n nn ...) (pred pp ...) ((frm body ...) next ...) count x)
     (if (pred count n)
       (apply (lambda frm body ...) x)
       (%%emit-case-lambda-body
         (nn ...)
         (pp ...)
         (next ...)
         count                                                                  
         x)))))
 
(define-syntax %gen-case-lambda-dotted-reverse-quote
  (syntax-rules ()
    ((_ acc (frm . (a . d)))
     (%gen-case-lambda-dotted-reverse-quote
       (frm . acc)
       (a . d)))
    ((_ acc (frm . rest))
     '(frm . acc))))

(define-syntax %%gen-case-lambda-dispatch ;; Generation loop
  (syntax-rules () 
    ;; Term
    ((_ n pred clauses)
     (lambda x
       (let ((count (length x)))
        (%%emit-case-lambda-body
          n
          pred
          clauses
          count
          x))))
    ;; (a b c)
    ((_ (n ...) (pred ...) (clauses ...) ((frms ...) . code) next ...)
     (let ((nn (length '(frms ...))))
      (%%gen-case-lambda-dispatch
        (n ... nn)
        (pred ... =)
        (clauses ... ((frms ...) . code))
        next ...)))
    ;; (a b . c)
    ((_ (n ...) (pred ...) (clauses ...) ((frms . rest) . code) next ...)
     (let ((nn (length (%gen-case-lambda-dotted-reverse-quote () (frms . rest)))))
      (%%gen-case-lambda-dispatch
        (n ... nn)
        (pred ... >=)
        (clauses ... ((frms . rest) . code))
        next ...)))
    ;; a -- catch-all
    ((_ (n ...) (pred ...) (clauses ...) (frms . code) next ...)
     ;; FIXME -- are we required to catch (next ...) == () ??
     (%%gen-case-lambda-dispatch
       (n ... #t)
       (pred ... #t)
       (clauses ... (frms . code))
       next ...))))
         
)

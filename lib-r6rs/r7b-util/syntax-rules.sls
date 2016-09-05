(library (r7b-util syntax-rules)
         (export syntax-rules)
         (import (rename (rnrs) (syntax-rules syntax-rules:r6)))

;; SRFI-46 style syntax-rules

;; FIXME: We should use with-syntax like:
;;   http://srfi.schemers.org/srfi-93/mail-archive/msg00024.html
(define-syntax syntax-rules
  (lambda (x)
    (define (filt elip x)
      (if (identifier? x)
        (cond ((free-identifier=? elip x) (syntax (... ...)))
              ((free-identifier=? (syntax (... ...)) x) (syntax bogus))
              (else x)
              )
        x))
    (define (emap elip in)
      (syntax-case in ()
        ((x . y) (cons (emap elip (syntax x))
                       (emap elip (syntax y))))
        (#(x ...) (list->vector (emap elip (syntax (x ...)))))
        (x (filt elip (syntax x)))))

    (syntax-case x ()
      ((_ (lit ...) (pat tmpl) ...) ;; short cut
       (syntax (syntax-rules:r6 (lit ...) (pat tmpl) ...)))
      ((_ elip (lit ...) (pat tmpl) ...)
       (with-syntax (((clause ...) (emap (syntax elip) (syntax ((pat tmpl) ...)))))
         (syntax (syntax-rules:r6 (lit ...) clause ...)))))))

         
)

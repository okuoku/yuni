(library (chez-r7b i9)
         (export define-record-type)
         (import (except (rnrs) define-record-type)
                 (rename (rnrs) (define-record-type define-record-type:r6)))


(define-syntax define-record-type
  (lambda (x)
    (syntax-case x ()
      ((_ nam (ctr formal ...) pred (fieldname fieldparam ...) ...)
       (with-syntax 
         (((r6-fields ...) 
           ;; Construct immutable or mutable fields depends on clause length
           (map (lambda (c) (if (= 2 (length c))
                              ;; FIXME: Omit R6RS syntax
                              #`(immutable . #,c)
                              #`(mutable . #,c)))
                #'((fieldname fieldparam ...) ...)))
          ((unreferenced ...)
           ;; Unreferenced names inside constructor formals
           (fold-left (lambda (cur e)
                        (if (not (find (lambda (h) (bound-identifier=? e h))
                                       #'(formal ...)))
                          (cons e cur)
                          cur))
                      '()
                      #'(fieldname ...))))
         #'(define-record-type:r6 
             (nam ctr pred)
             ;; Constructor
             (protocol (lambda (c)
                         (lambda (formal ...)
                           ;; Instantiate unreferenced vars with undefined value
                           (define unreferenced)
                           ...
                           (c fieldname ...))))

             ;; Fields
             (fields r6-fields ...)

             ;; FIXME: Is that true?
             (sealed #t)))))))
         
)

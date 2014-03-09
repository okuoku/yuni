(library (yuni binary bitflags)
         (export define-bitflagset
                 integer->bitflags
                 bitflags->integer)
         (import (rnrs))

(define (bitflags->integer bitflagset lis)
  (fold-left (lambda (cur e)
               (if (integer? e)
                 (bitwise-or e cur)
                 (let* ((p (hashtable-ref bitflagset e #f)))
                   (unless (and p e)
                     (assertion-violation 'bitflags->integer
                                          "invalid argument"
                                          e
                                          p))
                   (bitwise-or p cur))))
             lis))

(define (integer->bitflags bitflagset i)
  (define residue i)
  (define x (fold-left (lambda (e p)
                         ;; FIXME: Abort if residue == 0
                         (let* ((name (car p))
                                (val (cdr p))
                                (mask (bitwise-not val))
                                (next (bitwise-and residue mask)))
                           (set! residue next)
                           (if (= 0 (bitwise-and val e))
                             e
                             (cons val e))))
                       '()
                       (hashtable-ref bitflagset #f)))
  (if (= residue 0)
    x
    (cons residue x)))

(define (gen-bitflagset lis)
  (define ret (make-eq-hashtable))
  (for-each (lambda (p) (let ((name (car p))
                              (val (cdr p)))
                          (hashtable-set! ret name val)))
            lis)
  (hashtable-set! #f lis)
  ret)
         
(define-syntax define-bitflagset
  (syntax-rules ()
    ((_ name ((name value) ...))
     (define name (gen-bitflagset `((name . ,value) ...))))))

)

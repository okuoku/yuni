(library (yunife import-set)
         (export
           make-iset
           iset-size
           ;; These procedures have ! but current implementation won't
           ;; mutate arguments
           iset-add!
           iset-filter!
           iset-index!
           iset-for-each

           ient-library-name
           ient-basename
           ient-imported-name
           ient-global-name
           ient-macro-code
           ient-macro-import
           ient-index
           )
         (import (yuni scheme))

;; FIXME: Use hashtable...
(define (make-iset) '())

(define (ient-library-name ient)
  (vector-ref ient 0))
(define (ient-basename ient)
  (vector-ref ient 1))
(define (ient-imported-name ient)
  (vector-ref ient 2))
(define (ient-macro-code ient)
  (vector-ref ient 3))
(define (ient-macro-import ient)
  (vector-ref ient 4))
(define (ient-macro-import-set! ient obj) ;; internal
  (vector-set! ient 4 obj) )
(define (ient-global-name ient)
  (vector-ref ient 5))
(define (ient-index ient)
  (vector-ref ient 6))

(define (iset-filter! iset thunk)
  (define (itr cur rest)
    (if (pair? rest)
      (let ((a (car rest))
            (next (cdr rest)))
        (if (thunk a)
          (itr (cons a cur) next)
          (itr cur next)) )
      cur))
  (itr '() iset))

(define (iset-index! iset)
  (define width (iset-size iset))
  (define global-name-table (make-vector width #f))
  (define (lookup sym)
    (define (itr i)
      (and (not (= width i))
           (or (eq? (vector-ref global-name-table i) sym)
               (itr (+ 1 i)))))
    (itr 0))
  (define (construct)
    (define (itr i rest)
      (unless (= width i)
        (vector-set! global-name-table 
                     (ient-global-name (car rest)))
        (itr (+ i 1) (cdr rest))))
    (itr 0 iset))

  ;; First, construct global-name-table
  (construct)

  ;; Replace macro environment with index
  (for-each 
    (lambda (ient)
      (let ((m (ient-macro-import ient)))
       (when m
         (ient-macro-import-set! 
           ient
           (map (lambda (e)
                  (let ((local-name (car e))
                        (global-name (cdr e)))
                    (let ((idx (lookup global-name)))
                     (unless idx
                       (error "Macro environment references unknown name"
                              ient))
                     (cons local-name idx))))
                m)))))
    iset))

(define (iset-size iset) (length iset))

(define (iset-add! iset library-name 
                   ;; name can be #f (internal def. etc.)
                   basename imported-name global-name
                   ;; Macro code/import can be #f
                   macro-code macro-import)
  (define (do-add)
    (define v (vector library-name basename imported-name 
                      macro-code macro-import global-name
                      ;; index, added on iset-index!
                      #f))
    (cons v iset))
  (define (check)
    ;; Check for dup.
    #t)
  (if (check)
    (do-add)
    iset))
(define (iset-for-each iset thunk) (for-each thunk iset))

)

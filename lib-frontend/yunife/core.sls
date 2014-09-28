;; 
;; alexpander adapter
;; 

(library (yunife core)
         (export 
           expand-code
           null-eset)
         (import (yuni scheme)
                 (yuni base match)
                 (yunife export-set)
                 (yunife import-set)
                 (yunife alexpander))
         
;;
;; Data format => (yunife import-set)/(yunife export-set)
;;
;;; import-set: Combined imported symbol set + macro code
;;;   #(library-name basename imported-name 
;;;     global-name?
;;;     macro-code? macro-import?)
;;;
;;;     global-name == #f => internal syntax/definition
;;;     global-name == #t => primitive syntax/definition
;;;     

;; Alexpander adapter

;;;; Filter built-ins
;;;;  NB: We check ... and _ was properly renamed into built-in itself
;;;;      Because Alexpander uses unbound aux-syntax rule but R6RS/R7RS use
;;;       bound aux-syntax. This also applies unquote and unquote-splice.
(define (filter-special-aux! iset)
  (define (special-aux? sym)
    (case sym
      ((... _ unquote unquote-splice) #t)
      (else #f)))
  (define (check ient)
    (define iname (ient-imported-name ient))
    (define bname (ient-basename ient))
    (define gname (ient-global-name ient))
    (cond ((special-aux? bname)
           ;; Error check
           (unless (eq? bname iname)
             (error "Cannot rename special aux-syntax" 
                    (ient-library-name ient)
                    bname iname))
           (unless (eq? #t gname)
             (error "Special aux-syntax cannot have global name"
                    gname) 
             ;; Remove it from the entry
             #f))
          (else #t)))
  (iset-filter! iset check))

;;;; convert imported symbol-set into Alexpander mstore
(define (iset->mstore iset)
  ;; FIXME: Be more functional...
  (define result '())
  (define (push! obj) (set! result (cons obj result)))
  (define (syntax-or-coresymbol? ient)
    (or (ient-macro-code ient) (eq? #t (ient-global-name ient))))
  ;; FIXME: move this out..?
  (iset-index! iset)
  (filter-special-aux! iset)
  ;; Generate variable exports
  (iset-for-each 
    iset
    ;; FIXME: Do actual rename here... 
    ;;        Actual rename will be performed as (letrec ...) output for now.
    (lambda (e)
      (unless (syntax-or-coresymbol? e)
        (let ((name (ient-imported-name e)))
         (push! (cons (ient-index e) (ient-imported-name e)))
         (push! (cons name (vector name)))))))
  ;; Generate macro/core-syntax exports
  (iset-for-each
    iset
    (lambda (e)
      (when (syntax-or-coresymbol? e)
        (let ((name (ient-imported-name e)))
         (cond
           (name ;; Have some external name
             (push! (cons (ient-index e) (ient-imported-name e)))
             (if (eq? #t (ient-global-name e))
               (push! (cons name name))
               (push! (cons name (list (ient-macro-code e)
                                       (ient-macro-import e)))))) 
           (else
             (push! (cons (ient-index e) (list (ient-macro-code e)
                                               (ient-macro-import e))))))))))

  ;; Construct mstore
  (cons result (iset-size iset)))

(define (null-eset)
  ;; Generate core-library export-set from null-mstore
  (define mstore (null-mstore))
  (define out (make-builtin-eset))
  out)

(define (expand-code code iset export-set-name?)
  (define mstore (iset->mstore iset))
  (define import-count (mstore-size mstore))
  (define eset (and export-set-name? (make-eset export-set-name?)))
  (let ((src (expand-top-level-forms! code mstore)))
   (when eset
     ;; Construct exported macro set
     'FIXME-do-nothing-for-now)
   (values src eset)))
         
;; Alexpander internals accessor

;;; MSTORE(mutable store) QUERY

(define (mstore-size mstore)
  (cdr mstore))

(define (mstore-store mstore)
  (car mstore))

)

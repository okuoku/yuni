(library (yuni ffi scmtemplate layouts)
         (export
           get-scm-libname/layouts
           put-scm-stubsource/layouts
           )
         (import (yuni scheme)
                 (yuni base match)
                 (yuni ffi scmtemplate root)
                 (yuni ffi ctemplate util)
                 (yuni ffi database flatten)
                 ;; FIXME: R6RS
                 (only (rnrs) filter))

;; layouts         

(define (get-scm-libname/layouts db)
  (append-libname (get-scm-libname db) 'layouts))

(define (field-param-name field-name)
  (string-append "c:" field-name))

(define (field-layout-name field-name)
  (string-append "%" field-name))

;; 
;; expand-flatten : expand aggregate entries and layouts into
;;                  intermediate format.
;;  
;;   entries ::= (<entry> ...)
;;   
;;   entry ::= (export? "layout-name" "layout-param-name"
;;              <aggregate-entry> ...)
;;   aggregate-entry ::=
;;             ("field-name" "field-param-name" #f)
;;             ("field-name" "field-param-name" "field-layout-name")
;;
(define (expand-flatten flat)
  (define entries '())
  (define (push-entry! x) (set! entries (cons x entries)))
  (define (flatten-layout? e) (eq? 'aggregate-entry (car e)))
  (define (flatten-aggregate-entry? e) (eq? 'aggregate-entry (car e)))
  (define aggregate-path* (map take-path-first
                               (filter flatten-aggregate-entry? flat)))
  (define (take-path-first entry)
    (match entry
           (('aggregate-entry label type parent cref sizeof offset 
             path . bogus)
            (list path label parent))))
  (define (gentree myname field-param-name cur depth)
    (define export? (= depth 2))
    (define acc '())
    ;;
    ;;       Next ::= (<next-entry> ...)
    ;; next-entry ::= ((path ...) "layout-name" "layout-param-name"
    ;;                 "label" ...)
    ;;
    (define next '())
    (define (add-next! e)
      (define mypath (car e))
      (define mylabel (cadr e))
      (define (itr acc cur)
        (cond
          ((null? cur)
           ;; Add a new entry
           (let ((parentpath (reverse (cdr (reverse mypath))))))
           )
          
          )
        
        )
      (set! next (itr '() next)))
    (define (pathfirst->entry e)
      (match e
             ((path label parent)
              (list label (field-param-name label)
                    XXXXXXX
                    ))))
    (for-each (lambda (e)
                (let ((pathlen (length (car e))))
                 (cond
                   ((= path depth)
                    (set! acc (cons e acc)))
                   (else
                     (add-next! e)))))
              cur)
    (push-entry!
      (append (list  export? myname field-param-name)
              (map pathfirst->entry acc)))
    (unless (null? next)
      )
    )
  (define (take-layout e)
    (define myname (match e
                          (('layout label . bogus)
                           label)))
    (define (child-node? e)
      (match e
             ((path label parent)
              (string=? parent myname))))
    (define nodes (filter child-node? aggregate-path*))
    (cond
      ((null? nodes)
       ;; No child nodes = opaque structure
       (push-entry! (list #t myname (field-param-name myname))))
      (else
        ;; Begin with (parent child) = length 2 case
        (gentree myname (field-param-name myname) nodes 2))))
  (for-each take-layout (filter flatten-layout? flat))
  entry)

(define (put-scm-stubsource/layouts port db)
  (define (p . obj) (apply put-obj port (append obj (list "\n"))))
  (define (dropname x) (or (and x (cadr x)) '()))
  (define flat (dropname (database->flatten/constants db)))
  (define entries (expand-flatten flat))

  ; header
  (p "(library " (get-scm-libname/layouts db))
  ; export
  (p "(export ")
  (for-each (lambda (sym) (p sym)) syms)
  (p ")")
  ; import
  (p "(import (yuni ffi runtime yunistub-layouts) " 
     (get-scm-libname/constants db) ")")
  ; body
  (p "(define-library-state *LIBSTATE* \"" c-dllname "\" \""
     c-libname "\")")
  (p)
  (emit-define-constants port "*LIBSTATE*" flat)
  (emit-bridgestub-constants port "*LIBSTATE*" flat2)
  (p ")"))


)

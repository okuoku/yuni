(library (yuni ffi runtime yunistub-layout)
         (export define-layout0*
                 define-layout-param
                 define-aggregate-entry-param)
         (import (yuni scheme)
                 (yuni base match)
                 (yuni base dispatch)
                 (yuni ffi runtime yunistub-procs-layout)
                 (yuni miniobj minidispatch))

;; Layout parameter definitions

(define-syntax define-layout-param
  (syntax-rules ()
    ((_ name sizeof typesym)
     (define name (and sizeof (list 0 sizeof 'typesym))))))

(define-syntax define-aggregate-entry-param
  (syntax-rules ()
    ((_ name param typesym)
     (define name (and param (list (cdr param) (car param) 'typesym))))))

;; Layout class definitions
;;  see (yuni ffi runtime yunistub-procs-layout) for actual implementation

(define-syntax gen-field-proc
  (syntax-rules ()
    ((_ field-name param #f)
     (%expand-param param gen-field-proc/term field-name #f))
    ((_ field-name param child)
     (%expand-param param gen-field-proc/child field-name child))))

(define-syntax gen-dispatch-func-output
  (syntax-rules ()
    ((_ ((field-name func) ...))
     (lambda (field . cmd)
       (case field
         ((field-name) (apply func cmd))
         ...)))))

(define-syntax gen-dispatch-func-itr
  (syntax-rules ()
    ((_ (cur ...) ((field-name param) next ...))
     (gen-dispatch-func-itr (cur ...) ((field-name param #f) next ...)))
    ((_ (cur ...) ((field-name param child) next ...))
     (let ((func (make-field-proc 'field-name param child)))
      (gen-dispatch-func-itr
        (cur ... (field-name func))
        (next ...))))
    ((_ (cur ...) ())
     (gen-dispatch-func-output (cur ...)))))

(define-syntax gen-dispatch-func
  (syntax-rules ()
    ((_ param ...)
     (gen-dispatch-func-itr () (param ...)))))

(define-syntax define-layout0*
  (syntax-rules ()
    ((_ name root-param param ...)
     (define-minidispatch-class
       name
       (make-layout-proc root-param
                         (gen-dispatch-func
                           param ...))))))
         
)

(library (yuni ffi ctemplate constants)
         (export
           put-c-constants)
         (import (yuni scheme)
                 (yuni base match)
                 (yuni ffi ctemplate util)
                 (yuni ffi database flatten)

                 ;; FIXME: R6RS
                 (only (rnrs)
                       make-hashtable
                       string-hash
                       hashtable-ref
                       hashtable-set!
                       )
                 ) 

;; c-template: constants
;; 
;;  constants = typelayout(size-of and offset-of) + "real" constants

;;; Part of ABI
(define (export-func-name name)
  (string-append name "_export_constants"))

;;; C Macro (not part of ABI)
(define (export-macro-name name)
  (define (uscfy str)
    ;; Convert / to _
    (string-map (lambda (c) (if (char=? c #\/) #\_ c))
                str))

  (string-append "EXPORT_" (uscfy name)))

(define (export-macro-cclass cclass)
  (case cclass
    ((value) "")
    ((c-struct) "struct ")
    ((c-enum) "enum ")
    ((c-union) "union ")
    (else
      (error "Invalid cclass" cclass))))

(define (export-macro-const ctype label)
  (string-append
    (case ctype
      ((void)
       (error "Are you sure want to export a void type?"))
      ((signed) "YUNIFFI_EXPORTCLASS_CONST_SINT")
      ((unsigned) "YUNIFFI_EXPORTCLASS_CONST_UINT")
      ((pointer) "YUNIFFI_EXPORTCLASS_CONST_PTR")
      ((real) "YUNIFFI_EXPORTCLASS_CONST_REAL")
      ((blob) "YUNIFFI_EXPORTCLASS_CONST_BLOB")
      (else
        (error "Invalid ctype" ctype)))
    "(k, \"" label "\", " label ")"))

(define (export-macro-aggregate cclass cref parent label)
  (string-append
    "YUNIFFI_EXPORTCLASS_AGGREGATE_MEMBER"
    "(k, \"" label "\", " (export-macro-cclass cclass)
    parent ", " cref ")"))

(define (export-macro-layout cclass label)
  ;; FIXME: Don't we need YUNIFFI_EXPORTCLASS_TYPE_BLOB??
  (string-append
    "YUNIFFI_EXPORTCLASS_TYPE(k, \""
    label
    "\", " 
    (export-macro-cclass cclass)
    label ")"))

(define (put-c-constants port db)
  (define flat (database->flatten/constants db))
  (define (p . obj) (apply put-obj port (append obj (list "\n"))))
  (define cclass-ht (make-hashtable string-hash string=?))
  (define (cache-cclass! e)
    (match e
           (('layout label ctype cclass sizeof . _)
            (hashtable-set! cclass-ht label cclass))
           (else 'do-nothing)))
  (define (resolve-cclass str)
    (or (hashtable-ref cclass-ht str #f)
        (error "Unknown parent for cclass" str)))

  (define (proc-ifdef thunk e)
    (define (do-attr attr)
      (cond
        ((pair? attr)
         (let ((cur (car attr))
               (next (lambda () (do-attr (cdr attr)))))

           (match cur
                  (('ifdef macro)
                   (p "#ifdef " macro)
                   (next)
                   (p "#endif /* " macro " */"))
                  (else
                    ;; Do nothing special
                    (next)))))
        (else (thunk))))
    (match e
           (('constant label ctype value sizeof . attr)
            (do-attr attr)
            )
           (('layout label ctype cclass sizeof . attr)
            (do-attr attr)
            )
           (('aggregate-entry label ctype parent cref 
             sizeof offset . attr)
            (do-attr attr))))

  (define (entry-macro e)
    ;; Emit EXPORT_xxx(k) macro. Attributes are ignored on here.
    (match e
           (('constant label ctype value sizeof . _)
            (p "#define " (export-macro-name label) "(k)\\")
            (p "    " (export-macro-const ctype label))
            (p ""))
           (('layout label ctype cclass sizeof . _)
            (p "#define " (export-macro-name label) "(k)\\")
            (p "    " (export-macro-layout cclass label))
            (p ""))
           (('aggregate-entry label ctype parent cref 
             sizeof offset . _)
            (p "#define " (export-macro-name label) "(k)\\")
            (p "    " (export-macro-aggregate 
                        (resolve-cclass parent) cref parent label))
            (p "" ))))

  (define (footer name)
    (p "YUNIFFI_EXPORTFUNC_END(" (export-func-name name) ")")
    (p "")
    (p "YUNIFFI_C_END"))
  (define (body-start name)
    (p "#include <yuniffi/stub/_begin_constants.h>")
    (p "")
    (p "YUNIFFI_C_BEGIN")
    (p "")
    (p "YUNIFFI_EXPORTFUNC_BEGIN(" (export-func-name name) ")"))
  (define (body entry)
    (define (itr idx cur)
      (cond
        ((pair? cur)
         (let ((a (car cur))
               (d (cdr cur)))
           (let ((label (cadr a)))
            (proc-ifdef
              (lambda ()
                (p "    "
                   "YUNIFFI_EXPORTFUNC_ENTRY("
                   idx
                   ","
                   (export-macro-name label)
                   ")"))
              a))
           (itr (+ 1 idx) d)))
        (else
          (p "    "
             "YUNIFFI_EXPORTFUNC_ENTRY_TERM("
             idx
             ")")) 
        ))
    (itr 1 entry))
  (match flat
         ((name)
          'do-nothing)
         ((name (entry ...))
          ;; Cache cclass for aggregate-entry
          (for-each cache-cclass! entry)
          (for-each entry-macro entry)
          (body-start name)
          (body entry)
          (footer name))))

)

(library (yuni ffi ctemplate functions)
         (export
           put-c-functions)
         (import (yuni scheme)
                 (yuni base match)
                 (yuni ffi ctemplate util)
                 (yuni ffi database flatten))

;; c-template: functions

;;; Part of ABI
(define (export-func-name name)
  (string-append name "_export_bridgestubs"))

;;; C names (not part of ABI)
(define (local-func-name stubtype name)
  (let ((sufx (case stubtype
                ((forward-0) "_forward0")
                ((forward-1) "_forward1")
                ((backward-1) "_backward1")
                ((backward-2) "_backward2")
                (else
                  (error "Unknown stub type" stubtype)))))
    (string-append "ystub_" name sufx)))

(define (forward-type-name name)
  (string-append name "_func_t"))

(define (export-macro-name stubtype name)
  (string-append "EXPORT_" (local-func-name stubtype name)))

(define (yuniword-ref-macro-name ctype)
  (case ctype
    ((pointer) "YUNIWORD_REF_PTR")
    ((signed) "YUNIWORD_REF_SINT")
    ((unsigned) "YUNIWORD_REF_UINT")
    ((real) "YUNIWORD_REF_DOUBLE")
    (else (error "Unknown ctype for ref" ctype))))

(define (yuniword-set-macro-name ctype)
  (case ctype
    ((pointer) "YUNIWORD_SET_PTR")
    ((signed) "YUNIWORD_SET_SINT")
    ((unsigned) "YUNIWORD_SET_UINT")
    ((real) "YUNIWORD_SET_DOUBLE")
    (else (error "Unknown ctype for set" ctype))))


(define (expand-stubtypes flat)
  (define (itr cur rest)
    (if (pair? rest)
      (let ((f (car rest))
            (next (cdr rest)))
        (match f
               ((funcname ret args stubtype)
                (itr (cons f cur) next))
               ((funcname ret args stubtype . nexttype)
                (itr (cons (list funcname ret args stubtype) cur)
                     (cons (cons funcname (cons ret (cons args nexttype))) 
                           next)))))
      (reverse cur)))
  (match flat
         ((name (funcs ...))
          (list name (itr '() funcs)))))

(define (attr-out? x) (memq 'out x))
(define (attr-in? x) (memq 'in x))
(define (split-args ret args) ;; => in* + out*
  (define in* '())
  (define out* '())
  (define (put! prefer-out? e)
    (match e
           ((ctype name cterm . attr)
            (cond
              ((attr-out? attr)
               (set! out* (cons e out*)))
              ((attr-in? attr)
               (set! in* (cons e in*)))
              (prefer-out?
                (set! out* (cons e out*)))
              (else
                (set! in* (cons e in*)))))))
  (for-each (lambda (e) (put! #t e)) ret)
  (for-each (lambda (e) (put! #f e)) args)
  (values (reverse in*) (reverse out*)))

(define (for-each1/index proc x)
  (define (itr idx x)
    (unless (null? x)
      (proc idx (car x))
      (itr (+ 1 idx) (cdr x))))
  (itr 0 x))

(define (put-c-functions port db)
  (define flat (expand-stubtypes (database->flatten/functions db)))
  (define (p . obj) (apply put-obj port (append obj (list "\n"))))
  (define (pt . obj) (apply p "    " obj))
  (define (export-begin name)
    (p "YUNIFFI_C_BEGIN")
    (p "")
    (p "YUNIFFI_EXPORTFUNC_BEGIN(" (export-func-name name) ")"))
  (define (export-end name)
    (p "YUNIFFI_EXPORTFUNC_END(" (export-func-name name) ")")
    (p "")
    (p "YUNIFFI_C_END"))

  (define (funcdef e)
    (define (funcbegin stubtype name)
      (p "static")
      (p "YUNIFFI_FUNC_BEGIN("
         (local-func-name stubtype name)
         ",in,in_size,out,out_size)"))
    (define (funcend stubtype name)
      (p "YUNIFFI_FUNC_END("
         (local-func-name stubtype name)
         ")")
      (p ""))
    (define (functypedef funcname ret args)
      (define (rettype)
        (match ret
               ((ctype false cterm . bogus)
                cterm)
               (else "void")))
      (define (genargs)
        (define (itr cur rest)
          (match rest
                 (((ctype name cterm . bogus) . next)
                  (if (null? next)
                    (reverse (cons cterm cur))
                    (itr (cons "," (cons cterm cur)) next)))))
        (if (null? args)
          "void" ;; no argument
          (itr '() args)))
      ;; Emit a typedef for forward-1 stub
      (apply pt
             `("typedef " ,(rettype)
               " (*"
               ,(forward-type-name funcname)
               ")("
               ,@(genargs) ");")))

    (define (funcbody funcname ret args stubtype)
      (define void-call? (null? ret))
      (define call-in0? (eq? stubtype 'forward-1))
      (define callname (if call-in0? "in0" funcname))
      (define (inargs idx e)
        (match e
               ((ctype name cterm . bogus)
                (pt "/* in[" idx "] : "
                    name
                    " */")
                (pt cterm " const in" idx " = "
                    "(" cterm ")"
                    (yuniword-ref-macro-name ctype)
                    "(in," idx ");"))))
      (define (outargs idx e)
        (match e
               ((ctype name cterm . bogus)
                (pt "/* out[" idx "] : "
                    (or name "return value")
                    " */")
                (pt cterm " out" idx ";"))))
      (define (output idx e)
        (match e
               ((ctype name cterm . bogus)
                (pt (yuniword-set-macro-name ctype)
                    "(out," idx ",out" idx ");" ))))
      (define (callargs)
        (define in-idx (if call-in0? 1 0))
        (define out-idx (if void-call? 0 1))
        (define ret '())
        (define (put! x)
          (if (null? ret)
            (set! ret (cons x ret))
            (set! ret (cons x (cons "," ret)))))
        (define (put-out!)
          (let ((nam (string-append "&out"
                                    (number->string out-idx))))
            (set! out-idx (+ 1 out-idx))
            (put! nam)))
        (define (put-in!)
          (let ((nam (string-append "in"
                                    (number->string in-idx))))
            (set! in-idx (+ 1 in-idx))
            (put! nam)))
        (define (proc a)
          (match a
                 ((ctype name cterm . attr)
                  (cond
                    ((attr-out? attr)
                     (put-out!))
                    (else
                      (put-in!))))))

        (for-each proc args)
        (reverse ret))

      (define-values (in* out*)
                     (split-args ret args))

      ;; Construct in def/entry part
      (for-each1/index inargs in*)
      (p)
      ;; Construct out def part
      (for-each1/index outargs out*)
      (p)
      ;; Construct call part
      (pt "/* call */")
      (cond
        (void-call?
          (apply pt callname "(" `(,@(callargs) ");") ))
        (else
          (apply pt "out0 = " callname "(" `(,@(callargs) ");") )))
      (p)
      ;; Construct output part
      (pt "/* output */")
      (for-each1/index output out*))

    (match e
           ((funcname ret args stubtype)
            (funcbegin stubtype funcname)
            (when (eq? stubtype 'forward-1)
              ;; We need function type
              (functypedef funcname ret args)
              ;; ... and insert func argument in front of args
              (set! args (cons (list 'pointer 
                                     "funcptr" 
                                     (forward-type-name funcname))
                               args)))
            (funcbody funcname ret args stubtype)

            (funcend stubtype funcname))))

  (define (entry-macro e)
    (define (exporttype stubtype)
      (case stubtype
        ((forward-0) "YUNIFFI_EXPORTPROC_F0")
        ((forward-1) "YUNIFFI_EXPORTPROC_F1")
        ((backward-1) "YUNIFFI_EXPORTPROC_B1")
        ((backward-2) "YUNIFFI_EXPORTPROC_B2")
        (else "UNKNOWN")))
    (match e
           ((funcname ret args stubtype)
            (p "#define "
               (export-macro-name stubtype funcname)
               "(k) \\")
            (pt (exporttype stubtype)
                "(k,"
                (local-func-name stubtype funcname)
                ")")
            (p))))
  (define (entry func*)
    (define (entryone cur e rest)
      (match e
             ((funcname ret args stubtype)
              (pt "YUNIFFI_EXPORTFUNC_ENTRY(" cur ","
                  (export-macro-name stubtype funcname)
                  ")")))
      (cond
        ((pair? rest)
         (entryone (+ 1 cur) (car rest) (cdr rest)))
        (else
          (pt "YUNIFFI_EXPORTFUNC_ENTRY_TERM(" (+ cur 1) ")"))))
    (when (pair? func*)
      (entryone 1 (car func*) (cdr func*))))
  (match flat
         ((name)
          'do-nothing)
         ((name (func ...))
          (for-each funcdef func)
          (for-each entry-macro func)
          (export-begin name)
          (entry func)
          (export-end name)))

  )


)

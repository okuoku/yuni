(define-syntax yuni/realize-library-hook
  (er-macro-transformer
    (lambda (e r c)
      (PCK 'REALIZE-HOOK: e)
      (let ((libname (cadr e))
            (promote? (caddr e)))
        (cond
          ((and (eq? 'yuni (car libname))
                (case (cadr libname)
                  ((scheme) #t)
                  (else #f)))
           (PCK 'SKIP: libname)
           '(begin))
          (else
            (let ((loadlib (lambda _ 
                             (let ((pth (yuni/library-name->path libname)))
                              (cond
                                (pth
                                  (PCK 'LOADING: pth)
                                  (load pth))
                                (else
                                  (PCK 'IGNORE libname)))))))
              (yuni/realize-library! loadlib libname promote?))))))))

(define-syntax import0
  (syntax-rules ()
    ((_ cls ...)
     (begin 
       (yuni/realize-library-hook cls #t)
       ...))))

(define-syntax %%yuni/expand-library
  (er-macro-transformer
    (lambda (e r c)
      (let ((libname (cadr e))
            (exports* (caddr e))
            (imports* (cadddr e))
            (body* (cadddr (cdr e))))
        (PCK 'EXPAND-LIBRARY: libname)
        (yuni/xform-library yuni/gensym
                            libname
                            exports*
                            imports*
                            body*)))))

(define-syntax library
  (syntax-rules ()
    ((_ libname (export exports ...) (import imports ...) . body)
     ;; FIXME: Check library form
     (%%yuni/expand-library libname (exports ...) (imports ...) body))))

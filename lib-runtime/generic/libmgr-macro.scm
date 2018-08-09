(define-macro (yuni/realize-library-hook libname)
  ;; FIXME: Implement this much more seriously
  ;; For s7
  (cond
    ((and (eq? 'yuni (car libname))
          (case (cadr libname)
            ((scheme) #t)
            (else #f)))
     (PCK 'SKIP: libname)
     '(begin))
    (else
      (let ((loadlib (lambda (bogus) 
                       (let ((pth (yuni/library-name->path libname)))
                        (cond
                          (pth
                            (PCK 'LOADING: pth)
                            (load pth))
                          (else
                            (PCK 'IGNORE libname)))))))
        (yuni/realize-library! loadlib libname #t)))))

(define-macro (import . import*)
  (cons 'begin
        (map (lambda (i) 
               (list 'yuni/realize-library-hook i))
             import*)))

(define-macro (library libname exports imports . body)
  (cond
    ((not (and (pair? libname)
               (pair? exports)
               (eq? 'export (car exports))
               (pair? imports)
               (eq? 'import (car imports))))
     (error "Malformed library" libname)))

  (for-each (lambda (i) 
              (let ((sym (car i)))
               (case sym
                 ((only except prefix rename)
                  (error "Unsupported op" i)))))
            (cdr imports))
  (let ((r (yuni/xform-library "FIXME: Rename procedure"
                               libname
                               (cdr exports)
                               (cdr imports)
                               body)))
    r))

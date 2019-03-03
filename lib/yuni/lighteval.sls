(library (yuni lighteval)
         (export 
           make-lighteval-env
           lighteval-bind
           lighteval-env-add-globals!
           lighteval-env-ref
           lighteval-env-set!)
         (import (yuni scheme)
                 (yuni hashtables)
                 (yuni compat lighteval))

(define (make-lighteval-env)
  (make-symbol-hashtable))         

(define (lighteval-env-ref env sym alt)
  (hashtable-ref env sym alt))

(define (lighteval-env-set! env sym obj)
  (hashtable-set! env sym obj))

(define (lighteval-env-add-globals! env alist)
  (let ((names (map car alist))
        (code (map (lambda (p)
                     (let ((name (car p))
                           (obj (cdr p)))
                       (list name obj)))
                   alist)))
    (let ((out (lighteval-bind env (list 'letrec* code (cons 'list names)))))
      (for-each (lambda (name obj)
                  (lighteval-env-set! env name obj))
                names
                out))))

(define (lighteval-bind env frm)
  (let-values (((k v) (hashtable-entries env)))
              (let* ((names (vector->list k))
                     (objs (vector->list v))
                     (code (list 'lambda names frm))
                     (proc (eval/yuni code)))
                (apply proc objs))))
         
)

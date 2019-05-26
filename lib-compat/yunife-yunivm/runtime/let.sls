;; Convert Scheme let syntaxes into yunivm letrec*
(library (yunife-yunivm runtime let)
         (export let let* letrec)
         (import (yunivm-core-syntax))


;; FIXME: fake
(define-syntax letrec
  (syntax-rules ()
    ((_ args ...)
     (letrec* args ...))))
         
(define-syntax let*
  (syntax-rules ()
    ((_ () body0 body1 ...)
     (letrec* () body0 body1 ...))
    ((_ ((var0 code0)) body0 body1 ...)
     (letrec* ((var0 code0)) body0 body1 ...))
    ((_ ((var0 code0) (var1 code1) ...) body0 body1 ...)
     (let ((var0 code0))
      (let* ((var1 code1) ...) body0 body1 ...)))))

(define-syntax let
  (syntax-rules ()
    ((_ () body0 body1 ...)
     (letrec* () body0 body1 ...))
    ((_ ((var code) ...) body0 body1 ...)
     ((lambda (var ...) body0 body1 ...)
      code ...))
    ((_ name ((var code) ...) body0 body1 ...)
     (letrec* ((name (lambda (var ...) body0 body1 ...)))
               (name code ...)))))         
         
)

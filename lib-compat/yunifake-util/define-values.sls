(library (yunifake-util define-values)
         (export define-values)
         (import)


(define-syntax $$define-values/output
  (syntax-rules ()
    ((_ ((acc . vars) ...) last #f expr)
     (begin
       (define acc #f)
       ...
       (define last 
         (call-with-values
           (lambda () expr)
           (lambda (vars ... lastvar)
             (set! acc vars)
             ...
             lastvar)))))
    ((_ () last #t expr)
     (define last
       (call-with-values
         (lambda () expr)
         (lambda lastvar lastvar))))
    ((_ ((acc . vars) ...) last #t expr)
     (begin
       (define acc #f)
       ...
       (define last
         (call-with-values
           (lambda () expr)
           (lambda (vars ... . lastvar)
             (set! acc vars)
             ...
             lastvar)))))))
         
(define-syntax $$define-values/itr
  (syntax-rules ()
    ((_ (acc ...) (frm . (next . out)) expr)
     ($$define-values/itr
       (acc ... (frm . temp))
       (next . out)
       expr))
    ((_ acc (frm) expr)
     ($$define-values/output
       acc
       frm
       #f
       expr))
    ((_ (acc ...) (frm . last) expr)
     ($$define-values/output
       (acc ... (frm . temp))
       last
       #t
       expr))
    ((_ acc frm expr)
     ($$define-values/output
       acc
       frm
       #t
       expr))))

(define-syntax define-values
  (syntax-rules ()
    ((_ frm expr)
     ($$define-values/itr
       () ;; acc
       frm expr))))
         
)

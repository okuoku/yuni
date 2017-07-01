(library (yunivm heap r7clib)
         (export make-r7clib)
         (import (yuni scheme)
                 (yunivm heap hostbridge)
                 (yunivm heap r7cfallback))

(define (make-r7clib coreops)
  (define fallback (make-r7cfallback coreops))
  (define hostbridge (make-hostbridge coreops))
  (define host (hostbridge 'HOST))
  (define target (hostbridge 'TARGET))
  (define co-car (coreops 'car))
  (define co-cdr (coreops 'cdr))
  (define co-null (coreops 'null))
  (define co-set-cdr! (coreops 'set-cdr!))
  (define co-cons (coreops 'cons))
  (define (co-caar x) (co-car (co-car x)))
  (define (co-cadr x) (co-car (co-cdr x)))
  (define (co-cdar x) (co-cdr (co-car x)))
  (define (co-cddr x) (co-cdr (co-cdr x)))
  (define co-true (coreops 'true))
  (define co-false (coreops 'false))
  (define co-unspecified (coreops 'unspecified))
  (define (predicate proc)
    (lambda (a b)
      (let ((r (proc a b)))
       (if r
         (co-true)
         (co-false)))))

  (define co-= (predicate =))
  (define co-<= (predicate <=))
  (define co->= (predicate >=))
  (define co-< (predicate <))
  (define co-> (predicate >))

  (define x-null? (coreops 'Pnull?))
  (define x-pair? (coreops 'Ppair?))
  (define x-eqv? (coreops 'Peqv?))

  (define (co-not obj)
    (if (x-eqv? (co-false) obj)
      (co-true)
      (co-false)))

  (define (argconv cur target-lis)
    (cond
      ((x-null? target-lis) (reverse cur))
      (else
        (let ((a (co-car target-lis))
              (d (co-cdr target-lis)))
          (argconv (cons a cur) d)))))

  (define (co-apply proc . args)
    (cond
      ((null? args)
       ;; short-cut
       (proc))
      (else
        (let ((r (reverse args)))
         (let ((a (argconv '() (car r)))
               (d (reverse (cdr r))))
           (apply proc (append d a)))))))

  (define (co-memv obj lis)
    (if (x-null? lis)
      (co-false)
      (if (x-pair? lis)
        (let ((a (co-car lis))
              (d (co-cdr lis)))
          (or (and (x-eqv? a obj) lis)
              (co-memv obj d)))
        (error "Pair required" lis))))

  (define ($fx-length/itr n lis)
    (cond
      ((x-null? lis) n)
      ((x-pair? lis)
       ($fx-length/itr (+ n 1) (co-cdr lis)))
      (else
        (error "Pair required" lis))))

  (define ($fx-length lis)
    ($fx-length/itr 0 lis))

  (define ($append/itr! cur lis) ;; => cur
    (cond
      ((x-null? lis) cur)
      ((x-pair? lis)
       (let* ((a (co-car lis))
              (d (co-cdr lis))
              (p (co-cons a (co-null)))) 
         (co-set-cdr! cur p)
         ($append/itr! p d)))
      (else
        (error "List required" lis))))

  (define ($append x y)
    (cond
      ((x-null? x) y)
      ((x-pair? x)
       (let* ((a (co-car x))
              (d (co-cdr x))
              (n (co-cons a (co-null))))
         (let ((m ($append/itr! n d)))
          (co-set-cdr! m y))
         n))
      (else
        (error "Pair required" x))))

  (define (func0 proc)
    (lambda args
      (let ((objs (map host args)))
       (apply proc objs)
       (co-unspecified))))

  (define (forbidden sym)
    (lambda args
      (error "Forbidden API for this heap" sym)))

  ;; (r7c core error)
  (define co-error (func0 error))
  ;; (r7c core callcc)
  ;; call-with-current-continuation call/cc
  (define co-call/cc (forbidden 'call/cc))
  ;; (r7c core exception)
  ;; raise raise-continuable with-exception-handler
  (define co-raise (forbidden 'raise))
  (define co-raise-continuable (forbidden 'raise-continuable))
  (define co-with-exception-handler (forbidden 'with-exception-handler))

  (define (query sym)
    (case sym
      ((;; (r7c heap pair)
        pair? cons car cdr set-car! set-cdr! null?
        ;; (r7c heap eqv)
        eqv?
        ;; (r7c heap core)
        eq?
        ;; (r7c heap eof-object)
        eof-object eof-object?
        ;; (r7c heap boolean)
        boolean?
        ;; (r7c heap char)
        char? char->integer integer->char
        ;; (r7c heap string)
        string? string-length string-ref string-set!
        ;; (r7c heap vector)
        vector? vector-length vector-ref vector-set!
        ;; (r7c heap bytevector)
        bytevector? bytevector-length bytevector-u8-ref bytevector-u8-set!
        ;; (r7c heap symbol)
        symbol? symbol->string string->symbol
        ;; (r7c-ext simple-struct)
        simple-struct? simple-struct-ref simple-struct-set!
        ;make-simple-struct0 
        simple-struct-name)
       ;; As-is
       (coreops sym))
      ;; FIXME
      ((make-simple-struct) (coreops 'make-simple-struct0))
      ;; Renames
      (($boolean=?) (coreops 'boolean=?/2))
      (($char=?) (coreops 'char=?/2))
      (($string=?) (coreops 'string=?/2))
      (($symbol=?) (coreops 'symbol=?/2))
      (($make-string) (coreops 'make-string0))
      (($make-vector) (coreops 'make-vector0))
      (($make-bytevector) (coreops 'make-bytevector0))
      (($undefined) (coreops 'undefined))
      ;; (r7c heap boolean)
      ((not) co-not)
      ;; (r7c heap pair)
      ((caar) co-caar)
      ((cadr) co-cadr)
      ((cdar) co-cdar)
      ((cddr) co-cddr)
      ;; (r7c heap procedure)
      ((procedure?) procedure?)

      ;; (r7c heap fixnum)
      (($fx=) co-=)
      (($fx<=) co-<=)
      (($fx>=) co->=)
      (($fx<) co-<)
      (($fx>) co->)
      ;; (r7c heap listloop)
      ((memv) co-memv)
      (($fx-length) $fx-length)
      (($append) $append)
      ;; (r7c core error)
      ((error) co-error)
      ((call/cc call-with-current-continuation) co-call/cc)
      ((raise) co-raise)
      ((raise-continuable) co-raise-continuable)
      ((with-exception-handler) co-with-exception-handler)
      ;; (r7c core apply)
      ((apply) co-apply)
      ;; (r7c core values)
      ((values) values)
      ((call-with-values) call-with-values)
      (else
        (fallback sym))))

  query)         
         
)

(define *yuni/libalias*
  '((yuni . s7-yuni)
    (scheme . s7-scheme)))
(require 'stuff.scm)
(define (yuni/gensym bogus) (gensym (symbol->string bogus)))
(define (yuni/identifier? x) (symbol? x))

(define-class simple-struct0 '()
              '((name 0)
                (v 0))
              '())

(define-macro (define-values vars expr)
  `(varlet (curlet)
          ((lambda ,vars (curlet)) ,expr)))

;; let-values (took from r7rs.scm)
(define-macro (let-values vars . body)
  (if (and (pair? vars)
           (pair? (car vars))
           (null? (cdar vars)))
    `((lambda ,(caar vars)
        ,@body)
      ,(cadar vars))
    `(with-let (apply sublet (curlet)
                      (list ,@(map (lambda (v)
                                     `((lambda ,(car v)
                                         (values ,@(map (lambda (name)
                                                          (values (symbol->keyword name) name))
                                                        (let args->proper-list ((args (car v)))
                                                         (cond ((symbol? args)	(list args))
                                                               ((not (pair? args))	args)
                                                               ((pair? (car args))	(cons (caar args) (args->proper-list (cdr args))))
                                                               (else                 (cons (car args) (args->proper-list (cdr args)))))))))
                                       ,(cadr v)))
                                   vars)))
               ,@body)))

;; case-lambda (took from r7rs.scm)
(define-macro (case-lambda . choices)
  `(lambda args
     (case (length args)
       ,@(map (lambda (choice)
                (if (or (symbol? (car choice))
                        (negative? (length (car choice))))
                  `(else (apply (lambda ,(car choice) ,@(cdr choice)) args))
                  `((,(length (car choice)))
                    (apply (lambda ,(car choice) ,@(cdr choice)) args))))
              choices))))

;; Parameters (took from r7rs.scm)
(define* (make-parameter init converter)
  (let* ((convert (or converter (lambda (x) x)))
         (old-values ()) ; see below -- this is part of the funclet
         (value (convert init)))
    (lambda () value)))

(define-macro (parameterize vars . body)
  `(dynamic-wind
       (lambda ()
         ,@(map (lambda (var)
                  `(with-let (funclet ,(car var))
                     (set! old-values (cons value old-values))
                     (set! value (convert ,(cadr var)))))
                vars))
       (lambda ()
         ,@body)
       (lambda ()
         ,@(map (lambda (var)
                  `(with-let (funclet ,(car var))
                     (set! value (car old-values))
                     (set! old-values (cdr old-values))))
                vars))))

;; Errors
;;  NB: Are diverged from r7rs.scm; we need some protocols to implement
;;      Yuni's own compatibility requirements for raise
(define (raise obj) 
  (if (pair? obj)
    (apply throw obj)
    (throw 'yuni/raise obj)))
(define (error msg . irr) (apply throw 'yuni/error msg irr))

(define-macro (guard results . body)
  (let ((condition (car results))
        (clauses (cdr results))
        (raw-condition (gensym "raw"))
        (result (gensym "result"))
        (here-result (gensym "here-result")))

    `(catch ',here-result
            (lambda ()
              (let ((,raw-condition 
                      (catch #t
                             (lambda () 
                               (let ((,result (let () ,@body)))
                                ;(write (list 'RESULT: ,result)) (newline)
                                (throw ',here-result ,result)))
                             (lambda (typ args)
                               ;(write (list 'DONE: typ args)) (newline)
                               (if (eq? typ ',here-result)
                                 (apply throw typ args)
                                 (cons typ args))))))
                ;; Unwrap raw-condition object before passing guard clauses
                (let ((,condition (if (eq? 'yuni/raise (car ,raw-condition))
                                    (cadr ,raw-condition)
                                    ,raw-condition)))
                  ;(write (list 'COND: ,condition)) (newline)
                  ;(write (list 'COND-IN: ',clauses)) (newline)
                  (cond
                    ,@clauses
                    ;; Rethrow if uncaught
                    (else 
                      ;(write (list 'RETHROW: ,raw-condition)) (newline)
                      (apply throw ,raw-condition))))))
            (lambda (type obj)
              ;(write (list 'RESULT2: type obj))(newline)
              (car obj)))))

(define (read-error? obj) (and (pair? obj) (eq? (car obj) 'read-error)))
(define (file-error? obj) (and (pair? obj) (eq? (car obj) 'io-error)))
(define (error-object-message obj) (apply format #f (cadr obj)))
(define error-object-irritants cdadr)

(define (error-object? obj) 
  (and (pair? obj) (symbol? (car obj))))

;; Lists/Pair
(define list-copy copy)

;; Vectors
(define ($vector-copy!+ to at from start end)
  ;; Forward
  (do ((i at (+ i 1))
       (j start (+ j 1)))
    ((= j end) to)
    (set! (to i) (from j))))
(define ($vector-copy!- to at from start end)
  ;; Backward
  (do ((i (- (+ at end) start 1) (- i 1))
       (j (- end 1) (- j 1)))
    ((< j start) to)
    (set! (to i) (from j))))

(define ($vector-copy! to at from start end)
  (cond
    ((or (eq? to from)
         (> at start))
     ($vector-copy!- to at from start end))
    (else
      ($vector-copy!+ to at from start end))))

(define (vector-copy! to at from . args)
  (if (null? args)
    ($vector-copy! to at from 0 (length from))
    (let ((start (car args))
          (d (cdr args)))
      (if (null? d)
        ($vector-copy! to at from start (length from))
        ($vector-copy! to at from start (car d))))))
(define vector-for-each for-each)
(define (vector-map p . args) (apply vector (apply map p args)))
(define vector->string
  (case-lambda
    ((v) (vector->string v 0 (length v)))
    ((v start) (vector->string v start (length v)))
    ((v start end)
     (copy v (make-string (- end start)) start end))))

;; Strings
(define string->vector
  (case-lambda
    ((s) (string->vector s 0 (length s)))
    ((s start) (string->vector s start (length s)))
    ((s start end)
     (copy s (make-vector (- end start)) start end))))
(define vector-copy string->vector)
(define string-copy! vector-copy!)
(define string-for-each for-each)
(define (string-map p . args) (apply string (apply map p args)))



;; Bytevectors
(define (string->utf8 str) (string->byte-vector str))
(define string->utf8
  (case-lambda
    ((str) (string->byte-vector str))
    ((str start) (string->byte-vector (substring str start (length str))))
    ((str start end) (string->byte-vector (substring str start end)))))
(define utf8->string
  (case-lambda
    ((bv) (byte-vector->string bv))
    ((bv start) (byte-vector->string (bytevector-copy bv start)))
    ((bv start end) (byte-vector->string (bytevector-copy bv start end)))))
(define (eof-object) #<eof>)
(define bytevector-length length)
(define (bytevector-u8-ref bv idx) (bv idx))
(define bytevector byte-vector)
(define make-bytevector make-byte-vector)
(define (bytevector-u8-set! bv i x) (set! (bv i) x))
(define bytevector? byte-vector?)
(define (bytevector-append . args) (string->byte-vector 
                                     (apply string-append 
                                            (map byte-vector->string args))))
(define bytevector-copy
  (case-lambda
    ((bv) (string->byte-vector (byte-vector->string bv)))
    ((bv start) (string->byte-vector (substring (byte-vector->string bv) start)))
    ((bv start end) (string->byte-vector (substring (byte-vector->string bv) 
                                                    start end)))))
(define bytevector-copy! vector-copy!)
(define write-bytevector 
  (case-lambda
    ((s) (write-string (byte-vector->string s)))
    ((s p) (write-string (byte-vector->string s) p))
    ((s p b) (write-string (substring (byte-vector->string s) b (length s)) p))
    ((s p b e) (write-string (substring (byte-vector->string s) b e) p))))

;; Typed Equivalence
(define (eq=?-itr type? x l)
  (or (null? l)
      (and (type? (car l))
           (eq? x (car l))
           (eq=?-itr type? x (cdr l)))))

(define (boolean=? . args)
  (or (null? args)
      (and
        (boolean? (car args))
        (eq=?-itr boolean? (car args) (cdr args)))))

(define (symbol=? . args)
  (or (null? args)
      (and
        (symbol? (car args))
        (eq=?-itr symbol? (car args) (cdr args)))))

;; Maths
(define inexact exact->inexact)
(define exact inexact->exact)
(define (square x) (* x x))
(define truncate-quotient quotient)
(define truncate-remainder remainder)
(define floor-remainder modulo)
(define (floor-quotient x y) (floor (/ x y)))
(define (floor/ a b)
  (values (floor-quotient a b)
          (modulo a b)))
(define (truncate/ a b)
  (values (quotient a b)
          (remainder a b)))
(define (finite? x)
  (and (number? x) (not (or (nan? x) (infinite? x)))))
(define exact-integer? integer?)
(define (exact-integer-sqrt i) 
  (let ((sq (floor (sqrt i)))) 
   (values sq (- i (* sq sq)))))

;; I/O
(define (close-port p)
  (if (input-port? p)
    (close-input-port p)
    (close-output-port p)))
(define (call-with-port port proc) ((if (input-port? port) call-with-input-file call-with-output-file) port proc))
(define (port? p) (or (input-port? p) (output-port? p)))
(define (input-port-open? p) (not (port-closed? p)))
(define (output-port-open? p) (not (port-closed? p)))
(define binary-port? port?)
(define textual-port? port?)
(define (open-input-bytevector bv) (open-input-string (byte-vector->string bv)))
(define open-output-bytevector open-output-string)
(define open-binary-input-file open-input-file)
(define open-binary-output-file open-output-file)
(define read-u8 read-byte)
(define write-u8 write-byte)
(define u8-ready? char-ready?)
(define peek-u8 peek-char)
(define (get-output-bytevector port) (string->byte-vector (get-output-string port)))

;; read-bytevector: Behaviour differs from r7rs.scm
(define ($read-bytevector! bv port cur start end)
  (if (= cur end)
    (- cur start)
    (let ((c (read-u8 port)))
     (if (eof-object? c)
       (- cur start)
       (begin
         (set! (bv cur) c)
         ($read-bytevector! bv port (+ cur 1) start end))))))

(define read-bytevector!
  (case-lambda
    ((bv port) ($read-bytevector! bv port 0 0 (length bv)))
    ((bv port start) ($read-bytevector! bv port start start (length bv)))
    ((bv port start end) ($read-bytevector! bv port start start end))) )

(define ($read-bytevector k port)
  (let ((out (string->byte-vector (make-string k))))
   (read-bytevector! out port)
   out))

(define (read-bytevector k . port-?)
  (if (null? port-?)
    ($read-bytevector k (current-input-port))
    ($read-bytevector k (car port-?))))

(define write-simple write)

;; OVERRIDES

(define string-copy+ ;; s7 has string-copy primitive
  (case-lambda
    ((s) (string-copy+ s 0 (length s)))
    ((s start) (string-copy+ s start (length s)))
    ((s start end)
     (let ((dest (make-string (- end start))))
      (string-copy! dest 0 s start end)
      dest))))

(set! string-copy string-copy+)

(define list? proper-list?)

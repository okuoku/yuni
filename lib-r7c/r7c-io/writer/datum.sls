(library (r7c-io writer datum)
         (export
           write
           write-shared
           write-simple
           display)
         (import (r7c-basic syntax define)
                 (r7c-basic lib strings)
                 (r7c-system core)
                 (r7c heap fixnum)
                 (r7c heap char)
                 (r7c heap string)
                 (r7c syntax or)
                 (r7c syntax and)
                 (r7c syntax let)
                 (r7c syntax unless)
                 (r7c-io port defaults)
                 (r7c-io port objects))

(define (%out-cdr port write? obj)
  (unless (null? obj)
    (cond
      ((pair? obj)
       (write-char #\space port)
       (%out-root port write? (car obj))
       (%out-cdr port write? (cdr obj)))
      (else
        (write-string " . " port)
        (%out-root port write? obj)))))

(define (%out-vector port write? obj start end)
  (unless ($fx= start end)
    (unless ($fx= start 0)
      (write-char #\space port))
    (%out-root port write? (vector-ref obj start))
    (%out-vector port write? obj ($fx+ start 1) end)))

(define (%out-bytevector port obj start end)
  (unless ($fx= start end)
    (unless ($fx= start 0)
      (write-char #\space port))
    (write-string (number->string (bytevector-u8-ref obj start)) port)
    (%out-bytevector port obj ($fx+ start 1) end)))

(define (%out-string port obj start end)
  (unless ($fx= start end)
    (let* ((c (string-ref obj start))
           (i (char->integer c)))
      (case i
        ((7) (write-string "\\a" port))
        ((8) (write-string "\\b" port))
        ((9) (write-string "\\t" port))
        ((#xa) (write-string "\\n" port))
        ((#xd) (write-string "\\d" port))
        ((#x22) (write-string "\\\"" port))
        ((#x5c) (write-string "\\\\" port))
        ;((#x7c) (write-string "\\\|" port)) ;; Not compatible with R6RS
        (else (write-char c port))))
    (%out-string port obj ($fx+ start 1) end)))

(define (%out-root port write? obj)
  (cond
    ((null? obj)
     (write-string "()" port))
    ((eof-object? obj)
     (write-string "#<eof-object>" port))
    ((boolean? obj)
     (if obj
       (write-string "#t" port)
       (write-string "#f" port)))
    ((symbol? obj)
     (write-string (symbol->string obj) port))
    ((pair? obj)
     (cond
       ((and (symbol? (car obj))
             (pair? (cdr obj))
             (null? (cddr obj))
             (let ((e (car obj)))
              (or (eq? 'quote e)
                  (eq? 'quasiquote e)
                  (eq? 'unquote e)
                  (eq? 'unquote-splicing e))))
        ;; Special handling for quote family
        (case (car obj)
          ((quote) (write-char #\' port))
          ((quasiquote) (write-char #\` port))
          ((unquote) (write-char #\, port))
          ((unquote-splicing) (write-char #\, port)
                              (write-char #\@ port))
          (else (error "Unexpected")))
        (%out-root port write? (cadr obj)))
       (else
         (write-char #\( port)
         (%out-root port write? (car obj))
         (%out-cdr port write? (cdr obj))
         (write-char #\) port))))
    ((vector? obj)
     (write-char #\# port)
     (write-char #\( port)
     (%out-vector port write? obj 0 (vector-length obj))
     (write-char #\) port))
    ((bytevector? obj)
     (write-char #\# port)
     (write-char #\u port)
     (write-char #\8 port)
     (write-char #\( port)
     (%out-bytevector port obj 0 (bytevector-length obj))
     (write-char #\) port))
    ((string? obj)
     (cond
       (write?
         (write-char #\" port)
         (%out-string port obj 0 (string-length obj))
         (write-char #\" port))
       (else
         (write-string obj port))))
    ((number? obj)
     (write-string (number->string obj) port))
    ((char? obj)
     (cond
       (write? 
         (write-char #\# port)
         (write-char #\\ port)
         (write-char obj port))
       (else
         (write-char obj port))))
    ((procedure? obj)
     (write-string "#<procedure>"))
    ((port? obj)
     (write-string "#<port>"))
    (else
      (write-string "#<unknown>" port))))

(define (display obj . port?)
  (if (null? port?)
    (%out-root (current-output-port) #f obj)
    (%out-root (car port?) #f obj)))

(define (write obj . port?)
  (if (null? port?)
    (%out-root (current-output-port) #t obj)
    (%out-root (car port?) #t obj)))

(define write-simple write)
(define write-shared write)
         
)

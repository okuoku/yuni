(library (yunivm heap hostbridge)
         (export
           make-hostbridge)
         (import (yuni scheme))

;;

(define (make-hostbridge coreops)

  ;; P prefix means predicates that returns *host* boolean
  (define op-null (coreops 'null))
  (define op-null? (coreops 'Pnull?))
  (define op-eof-object (coreops 'eof-object))
  (define op-eof-object? (coreops 'Peof-object?))
  (define op-true (coreops 'true))
  (define op-false (coreops 'false))
  (define op-boolean? (coreops 'Pboolean?))
  (define op-true? (coreops 'Ptrue?))
  (define op-false? (coreops 'Pfalse?))
  (define op-char? (coreops 'Pchar?))
  (define op-integer->char (coreops 'integer->char))
  (define op-char->integer (coreops 'char->integer))
  (define op-string? (coreops 'Pstring?))
  (define op-string-length (coreops 'string-length))
  (define op-string-ref (coreops 'string-ref))
  (define op-string-set! (coreops 'string-set!))
  (define op-make-string0 (coreops 'make-string0))
  (define op-bytevector? (coreops 'Pbytevector?))
  (define op-bytevector-length (coreops 'bytevector-length))
  (define op-bytevector-u8-ref (coreops 'bytevector-u8-ref))
  (define op-bytevector-u8-set! (coreops 'bytevector-u8-set!))
  (define op-make-bytevector0 (coreops 'make-bytevector0))
  (define op-symbol? (coreops 'Psymbol?))
  (define op-string->symbol (coreops 'string->symbol))
  (define op-symbol->string (coreops 'symbol->string))
  (define op-pair? (coreops 'Ppair?))
  (define op-cons (coreops 'cons))
  (define op-car (coreops 'car))
  (define op-cdr (coreops 'cdr))
  (define op-vector? (coreops 'Pvector?))
  (define op-vector-length (coreops 'vector-length))
  (define op-vector-ref (coreops 'vector-ref))
  (define op-vector-set! (coreops 'vector-set!))
  (define op-make-vector0 (coreops 'make-vector0))
  (define op-simple-struct? (coreops 'Psimple-struct?))

  (define (hostcopy ref set len in out)
    (define (itr cur)
      (if (= cur len)
        out
        (let ((obj (host (ref in cur))))
         (set out cur obj)
         (itr (+ 1 cur)))))
    (itr 0))

  (define (host obj)
    (cond
      ;; As-is
      ((number? obj)
       obj)
      ((port? obj)
       obj)
      ((procedure? obj)
       obj)

      ;; Convert datum
      ((op-char? obj)
       (integer->char (op-char->integer obj)))
      ((op-null? obj) '())
      ((op-eof-object? obj) (eof-object))
      ((op-boolean? obj)
       (cond
         ((op-true? obj) #t)
         ((op-false? obj) #f)
         (else (error "Huh?"))))
      ((op-symbol? obj)
       (string->symbol (host (op-symbol->string obj))))

      ;; Convert structure
      ((op-pair? obj)
       (cons (host (op-car obj)) 
             (host (op-cdr obj))))
      ((op-vector? obj)
       (let* ((len (op-vector-length obj))
              (out (make-vector len)))
         (hostcopy op-vector-ref vector-set! len obj out)
         out))
      ((op-bytevector? obj)
       (let* ((len (op-bytevector-length obj))
              (out (make-bytevector len)))
         (hostcopy op-bytevector-u8-ref bytevector-u8-set! len obj out)
         out))
      ((op-string? obj)
       (let* ((len (op-string-length obj))
              (out (make-string len)))
         (hostcopy op-string-ref string-set! len obj out)
         out))


      ;; Forbidden
      ((op-simple-struct? obj)
       (error "Forbidden object" obj))
      (else
        (error "Unknown object" obj))))

  (define (targetcopy ref set len in out)
    (define (itr cur)
      (if (= cur len)
        out
        (let ((obj (target (ref in cur))))
         (set out cur obj)
         (itr (+ 1 cur)))))
    (itr 0))

  (define (target obj)
    (cond
      ;; As-is
      ((number? obj) obj)
      ((port? obj) obj)
      ((procedure? obj) obj)

      ;; Convert datum
      ((char? obj)
       (op-integer->char (char->integer obj)))
      ((null? obj)
       (op-null))
      ((eof-object? obj)
       (op-eof-object))
      ((boolean? obj)
       (if obj
         (op-true)
         (op-false)))
      ((symbol? obj)
       (op-string->symbol (target (symbol->string obj))))

      ;; Convert structure
      ((pair? obj)
       (op-cons (target (car obj))
                (target (cdr obj))))
      ((vector? obj)
       (let* ((len (vector-length obj))
              (out (op-make-vector0 len)))
         (targetcopy vector-ref op-vector-set! len obj out)
         out))
      ((bytevector? obj)
       (let* ((len (bytevector-length obj))
              (out (op-make-bytevector0 len)))
         (targetcopy bytevector-u8-ref op-bytevector-u8-set! len obj out)
         out))
      ((string? obj)
       (let* ((len (string-length obj))
              (out (op-make-string0 len)))
         (targetcopy string-ref op-string-set! len obj out)
         out))

      ;; Forbidden
      (else
        (error "Unknown object" obj))))

  (define (query sym)
    (case sym
      ((HOST) host)
      ((TARGET) target)
      (else
        (error "Unknown symbol" sym))))

  query)
         
         
)

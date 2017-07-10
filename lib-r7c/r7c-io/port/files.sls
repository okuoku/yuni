(library (r7c-io port files)
         (export
           open-input-file
           open-binary-input-file
           open-output-file
           open-binary-output-file)
         (import (r7c-basic syntax define)
                 (r7c-basic lib strings)
                 (r7c-system core)
                 (r7c heap fixnum)
                 (r7c heap char)
                 (r7c syntax or)
                 (r7c syntax and)
                 (r7c syntax let)
                 (r7c-yunicore yuniport))

         
(define (open-input-file fn)
  (define eof? #f)
  (define buf #f)
  (define open? #t)
  (define fh 
    (if (eq? 'stdin fn)
      (filehandle-stdin)
      (filehandle-open/input fn)))
  (define (close)
    (set! open? #f)
    (filehandle-close fh))
  (define (input-port-open?) open?)

  (define (fill!)
    (unless open?
      (error "Fatal: Tried to fill closed file"))
    (unless buf
      (let ((bv (make-bytevector 512)))
       (let ((r (filehandle-read! fh bv 0 512)))
        (cond
          (($fx= r 512)
           (let ((lastbyte (bytevector-u8-ref bv 511)))
            (when ($fx= lastbyte 13)
              ;; Read more byte if the buffer had CR on the last byte
              (let ((plusone (make-bytevector 513)))
               (bytevector-copy! plusone 0 bv 0 512)
               (let ((r (filehandle-read! fh plusone 512 1)))
                (when ($fx= r 1)
                  (set! bv plusone)))))))
          (($fx= r 0)
           (set! eof? #t))
          (else 
            (let ((lastbuf (make-bytevector r)))
             (bytevector-copy! lastbuf 0 bv 0 r)
             (set! bv lastbuf))))
        (cond
          (eof? (set! buf #f))
          (else (set! buf (open-input-string (utf8->string bv)))))))))

  (define (my-peek-char)
    (cond
      (eof? (eof-object))
      (else
        (fill!)
        (if eof?
          (eof-object)
          (peek-char buf)))))

  (define (my-read-char)
    (cond
      (eof? (eof-object))
      (else
        (fill!)
        (if eof?
          (eof-object)
          (read-char buf)))))
  
  (define (read-line/itr buf)
    (let ((c (my-read-char)))
     (unless (eof-object? c)
       (let ((i (char->integer c)))
        (case i
          ((13) ;; CR
           (let ((n (my-peek-char)))
            (unless (eof-object? n)
              (when ($fx= 10 (char->integer n))
                ;; Consume LF
                (my-read-char)))))
          ((10) ;; LF
           ;; Do nothing
           #t)
          (else
            (write-char c buf)
            (read-line/itr buf)))))))

  (define (my-read-line)
    (if (eof-object? (my-peek-char))
      (eof-object)
      (let ((buf (open-output-string)))
       (read-line/itr buf)
       (get-output-string buf)))) 

  (define (my-read-string k)
    ;; FIXME: Implement it
    (if (eof-object? (my-peek-char))
      (eof-object)
      (read-string k buf)))

  (define (query sym)
    (case sym
      ((textual-port?) #t)
      ((input-port?) #t)
      ((input-port-open?) input-port-open?)
      ((close) close)
      ((close-input-port) close)
      ((read-char) my-read-char)
      ((peek-char) my-peek-char)
      ((read-line) my-read-line)
      ((read-string) my-read-string)
      (else #f)))

  (make-yuniport query))         
         
(define (open-binary-input-file fn)
  (define eof? #f)
  (define buf #f)
  (define open? #t)
  (define fh (filehandle-open/input fn))
  (define (close)
    (set! open? #f)
    (filehandle-close fh))
  (define (input-port-open?) open?)

  (define (fill!)
    (unless open?
      (error "Fatal: Tried to fill closed file"))
    (unless buf
      (let ((bv (make-bytevector 512)))
       (let ((r (filehandle-read! fh bv 0 512)))
        (cond
          (($fx= r 0)
           (set! eof? #t))
          (($fx= r 512)
           ;; Do nothing
           #t)
          (else
            (let ((lastblock (make-bytevector r)))
             (bytevector-copy! lastblock 0 bv 0 r)
             (set! bv lastblock))))
        (cond
          (eof? (set! buf #f))
          (else (set! buf (open-input-bytevector bv))))))))

  (define (my-peek-u8)
    (cond
      (eof? (eof-object))
      (else
        (fill!)
        (if eof?
          (eof-object)
          (peek-u8 buf)))))

  (define (my-read-u8)
    (cond
      (eof? (eof-object))
      (else
        (fill!)
        (if eof?
          (eof-object)
          (read-u8 buf)))))

  (define (read-bytevector!/itr total out start end)
    (if (or ($fx= start end) (eof-object? (my-peek-u8)))
      total
      (let ((r (read-bytevector! out buf start end)))
       (if (eof-object? r)
         (read-bytevector!/itr total out start end)
         (read-bytevector!/itr ($fx+ total r) out ($fx+ start r) end)))))

  (define (my-read-bytevector! out start end)
    (if (eof-object? (my-peek-u8))
      (eof-object)
      (read-bytevector!/itr 0 out start end)))

  (define (query sym)
    (case sym
      ((binary-port?) #t)
      ((input-port?) #t)
      ((input-port-open?) input-port-open?)
      ((close) close)
      ((close-input-port) close)
      ((read-u8) my-read-u8)
      ((peek-u8) my-peek-u8)
      ((read-bytevector!) my-read-bytevector!)
      (else #f)))

  (make-yuniport query))

(define (open-output-file fn)
  (define buf #f)
  (define open? #t)
  (define fh 
    (cond
      ((eq? 'stdout fn)
       (filehandle-stdout))
      ((eq? 'stderr fn)
       (filehandle-stderr))
      (else (filehandle-open/output fn))))
  (define (close)
    (set! open? #f)
    (filehandle-close fh))
  (define (output-port-open?) open?)

  (define (flush)
    (filehandle-flush fh))

  (define (write-char c)
    (let ((bv (string->utf8 (make-string 1 c))))
     (filehandle-write fh bv 0 (bytevector-length bv))))

  (define (write-string str start end)
    (let ((bv (string->utf8 str start end)))
     (filehandle-write fh bv 0 (bytevector-length bv))))

  (define (query sym)
    (case sym
      ((textual-port?) #t)
      ((output-port?) #t)
      ((flush) flush)
      ((output-port-open?) output-port-open?)
      ((close) close)
      ((close-output-port) close)
      ((write-char) write-char)
      ((write-string) write-string)
      (else #f))) 

  (make-yuniport query))

(define (open-binary-output-file fn)
  (define buf #f)
  (define open? #t)
  (define fh (filehandle-open/output fn))
  (define (close)
    (set! open? #f)
    (filehandle-close fh))
  (define (output-port-open?) open?)

  (define (flush)
    (filehandle-flush fh))

  (define (write-u8 b)
    (let ((bv (make-bytevector 1 b)))
     (filehandle-write fh bv 0 1)))

  (define (write-bytevector bv start end)
    (filehandle-write fh bv start end))

  (define (query sym)
    (case sym
      ((binary-port?) #t)
      ((output-port?) #t)
      ((flush) flush)
      ((output-port-open?) output-port-open?)
      ((close) close)
      ((close-output-port) close)
      ((write-u8) write-u8)
      ((write-bytevector) write-bytevector)
      (else #f))) 

  (make-yuniport query))

)

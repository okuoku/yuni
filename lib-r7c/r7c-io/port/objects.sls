(library (r7c-io port objects)
         (export
           read-char
           peek-char
           read-line
           read-string
           read-u8
           peek-u8
           read-bytevector
           read-bytevector!
           newline
           write-char
           write-string
           write-u8
           write-bytevector)
         (import (r7c-basic syntax define)
                 (r7c-system core)
                 (r7c heap fixnum)
                 (r7c-yunicore yuniport)
                 (r7c-io port defaults))

         
(define (read-char . port?)
  (if (null? port?)
    (yuniport-read-char (current-input-port))
    (yuniport-read-char (car port?))))

(define (peek-char . port?)
  (if (null? port?)
    (yuniport-peek-char (current-input-port))
    (yuniport-peek-char (car port?))))

(define (read-line . port?)
  (if (null? port?)
    (yuniport-read-line (current-input-port))
    (yuniport-read-line (car port?))))

(define (read-string k . port?)
  (if (null? port?)
    (yuniport-read-string (current-input-port) k)
    (yuniport-read-string (car port?) k)))

(define (read-u8 . port?)
  (if (null? port?)
    (yuniport-read-u8 (current-input-port))
    (yuniport-read-u8 (car port?))))

(define (peek-u8 . port?)
  (if (null? port?)
    (yuniport-peek-u8 (current-input-port))
    (yuniport-peek-u8 (car port?))))

(define (read-bytevector k . port?)
  ;; FIXME: Be more efficient...
  (let ((port (if (null? port?) (current-input-port) (car port?)))
        (buf (make-bytevector k)))
    (let ((x (read-bytevector! buf port)))
     (cond
       ((eof-object? x)
        x)
       ((= k x)
        buf)
       (else
         (let ((r (make-bytevector x)))
          (bytevector-copy! r 0 buf 0 x)
          r))))))

(define (read-bytevector! bv . args)
  (if (null? args)
    (yuniport-read-bytevector! (current-input-port) bv 0 (bytevector-length bv))
    (let ((port (car args))
          (start? (cdr args)))
      (if (null? start?)
        (yuniport-read-bytevector! port bv 0 (bytevector-length bv))
        (let ((end? (cdr start?)))
         (if (null? end?)
           (yuniport-read-bytevector! port bv 
                                      (car start?) (bytevector-length bv))
           (yuniport-read-bytevector! port bv (car start?) (car end?))))))))

(define (newline . port?)
  (if (null? port?)
    (yuniport-write-char (current-output-port) #\newline)
    (yuniport-write-char (car port?) #\newline)))

(define (write-char c . port?)
  (if (null? port?)
    (yuniport-write-char (current-output-port) c)
    (yuniport-write-char (car port?) c)))

(define (write-string str . args)
  (if (null? args)
    (yuniport-write-string (current-output-port)
                           str 0 (string-length str))
    (let ((start? (cdr args)))
     (if (null? start?)
       (yuniport-write-string (car args) str 0 (string-length str))
       (let ((end? (cdr start?)))
        (if (null? end?)
          (yuniport-write-string (car args) str 
                                 (car start?) (string-length str))
          (yuniport-write-string (car args) str (car start?) (car end?))))))))

(define (write-u8 b . port?)
  (if (null? port?)
    (yuniport-write-u8 (current-output-port) b)
    (yuniport-write-u8 (car port?) b)))

(define (write-bytevector bv . port?)
  (if (null? port?)
    (yuniport-write-bytevector (current-output-port)
                               bv 0 (bytevector-length bv))
    (let ((start? (cdr port?)))
     (if (null? start?)
       (yuniport-write-bytevector (car port?) bv 0 (bytevector-length bv))
       (let ((end? (cdr start?)))
        (if (null? end?)
          (yuniport-write-bytevector (car port?) bv (car start?)
                                     (bytevector-length bv))
          (yuniport-write-bytevector (car port?) bv 
                                     (car start?) (car end?))))))))

)

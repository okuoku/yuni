(library (r7c-io port buffers)
         (export
           %skip-newline
           %find-newline
           
           open-input-string
           open-output-string
           get-output-string
           
           open-input-bytevector
           open-output-bytevector
           get-output-bytevector)
         (import (r7c-basic syntax define)
                 (r7c-basic lib strings)
                 (r7c-system core)
                 (r7c heap fixnum)
                 (r7c heap char)
                 (r7c syntax or)
                 (r7c syntax and)
                 (r7c syntax let)
                 (r7c-yunicore yuniport))

         
;;

(define (%find-newline str start end)  ;; #f if the string teminated without NL
  (if ($fx= start end)
    #f
    (case (char->integer (string-ref str start))
      ((10 13) ;; CR or LF
       start)
      (else
        (%find-newline str ($fx+ 1 start) end)))))

(define (%skip-newline str start end) ;; #f if the string ends with newline
  (if ($fx= start end)
    #f
    (case (char->integer (string-ref str start))
      ((13) ;; CR
       ;; Find lf if available
       (let ((n ($fx+ 1 start)))
        (cond
          (($fx= n end) 
           ;; The string ends with LF
           #f)
          (else
            (if ($fx= 10 (char->integer (string-ref str n)))
              n ;; CR + LF
              start ;; CR
              )))))
      ((10) ;; LF
       (let ((n ($fx+ 1 start)))
        (if ($fx= n end)
          #f
          n)) )
      (else
        start))))

(define (open-input-string str)
  (define open? #t)
  (define ptr 0)
  (define len (string-length str))

  (define (close) (set! open? #f))
  (define (input-port-open?) open?)
  (define (peek-char)
    (if ($fx< ptr len)
      (string-ref str ptr)
      (eof-object)))
  (define (read-char)
    (if ($fx< ptr len)
      (let ((c (string-ref str ptr)))
       (set! ptr ($fx+ 1 ptr))
       c)
      (eof-object)))
  (define (read-line)
    (if ($fx>= ptr len)
      (eof-object)
      (let ((start (%skip-newline str ptr len)))
       (cond
         (($fx= ptr start)
          (let ((end (%find-newline str ptr len)))
           (cond
             (end
               ;; Skip CRLF
               (let ((next (%skip-newline str end len)))
                (set! ptr (if next next len)))
               ;; Return string
               (substring str start end))
             (else
               ;; String terminated
               (set! ptr len)
               ;; Return string
               (substring str start len)))))
         ((eqv? #f start)
          (set! ptr len)
          (make-string 0))
         (else
           (set! ptr start)
           (make-string 0))))))
  (define (read-string k)
    (let ((end? ($fx+ k ptr)))
     (let ((start ptr)
           (next (if ($fx< end? len) end? len)))
      (set! ptr next)
      (substring str start next))))

  (define (query sym)
    (case sym
      ((textual-port?) #t)
      ((buffer-port?) #t)
      ((input-port?) #t)
      ((input-port-open?) input-port-open?)
      ((close) close)
      ((close-input-port) close)
      ((read-char) read-char)
      ((peek-char) peek-char)
      ((read-line) read-line)
      ((read-string) read-string)
      (else #f)))
  (make-yuniport query))

(define (open-output-string)
  (define open? #t)
  (define buf "")
  (define (output-port-open?) open?)
  (define (close) (set! open? #f))
  (define (get-buffer)
    (let ((r buf))
     (set! buf "")
     r))
  (define (write-char c)
    (set! buf (string-append buf (make-string 1 c))))
  (define (write-string str start end)
    (set! buf (string-append buf (substring str start end))))

  (define (flush) 
    ;; Do nothing
    #t)
  (define (query sym)
    (case sym
      ((textual-port?) #t)
      ((buffer-port?) #t)
      ((output-port?) #t)
      ((flush) flush)
      ((output-port-open?) output-port-open?)
      ((close) close)
      ((close-output-port) close)
      ((get-buffer) get-buffer)
      ((write-char) write-char)
      ((write-string) write-string)
      (else #f)))

  (make-yuniport query))
         
(define (open-input-bytevector bv)
  (define ptr 0)
  (define len (bytevector-length bv))
  (define open? #t)
  (define (close) (set! open? #f))
  (define (input-port-open?) open?)

  (define (peek-u8)
    (if ($fx= len ptr)
      (eof-object)
      (bytevector-u8-ref bv ptr)))
  (define (read-u8)
    (if ($fx= len ptr)
      (eof-object)
      (begin
        (let ((b (bytevector-u8-ref bv ptr)))
         (set! ptr ($fx+ 1 ptr))
         b))))

  (define (read-bytevector! out start end)
    (let ((copylen ($fx- end start)))
     (let ((term ($fx+ ptr copylen)))
      (cond
        (($fx< len term)
         ;; Short copy case
         (let ((copylen2 ($fx- len ptr)))
          (cond
            (($fx= 0 copylen2)
             (set! ptr len)
             (eof-object))
            (else
              (bytevector-copy! out start bv ptr len)
              (set! ptr len)
              copylen2))))
        (else
          (bytevector-copy! out start bv ptr term)
          (set! ptr term)
          copylen)))))
  
  (define (query sym)
    (case sym
      ((binary-port?) #t)
      ((buffer-port?) #t)
      ((input-port?) #t)
      ((input-port-open?) input-port-open?)
      ((close) close)
      ((close-input-port) close)
      ((read-u8) read-u8)
      ((peek-u8) peek-u8)
      ((read-bytevector!) read-bytevector!)
      (else #f)))
  (make-yuniport query))

(define (open-output-bytevector)
  (define open? #t)
  (define buf (make-bytevector 0))
  (define (output-port-open?) open?)
  (define (close) (set! open? #f))
  (define (get-buffer)
    (let ((r buf))
     (set! buf (make-bytevector 0))
     r))
  (define (write-u8 b)
    (set! buf (bytevector-append buf (make-bytevector 1 b))))
  (define (write-bytevector bv start end)
    (let ((len1 (bytevector-length buf))
          (len2 ($fx- end start)))
      (let ((nextbuf (make-bytevector ($fx+ len1 len2))))
       (bytevector-copy! nextbuf 0 buf 0 len1)
       (bytevector-copy! nextbuf len1 bv start end)
       (set! buf nextbuf))))

  (define (flush) 
    ;; Do nothing
    #t)
  (define (query sym)
    (case sym
      ((textual-port?) #t)
      ((buffer-port?) #t)
      ((output-port?) #t)
      ((flush) flush)
      ((output-port-open?) output-port-open?)
      ((close) close)
      ((close-output-port) close)
      ((get-buffer) get-buffer)
      ((write-u8) write-u8)
      ((write-bytevector) write-bytevector)
      (else #f)))

  (make-yuniport query))

(define get-output-string yuniport-get-buffer)
(define get-output-bytevector yuniport-get-buffer)

)

;; I/O
(import (yuni scheme))

(define test-counter 0)
(define success-counter 0)
(define failed-forms '())

(define (check-finish)
  (display "Test: ")
  (display success-counter)
  (display "/")
  (display test-counter)
  (display " passed.\n")
  (unless (null? failed-forms)
    (display "\nFailed: \n")
    (for-each (lambda x
                (display "    ")
                (write x)
                (display "\n")
                ;; YuniVM: result convertion workaround
                #t)
              (reverse failed-forms)))
  ;; FIXME: Restore this later
  ;(flush-output-port (current-output-port))
  (exit (if (null? failed-forms) 0 1)))

(define-syntax check-equal
  (syntax-rules ()
    ((_ obj form)
     (begin
       ;(display (list 'obj 'form)) (newline)
       (set! test-counter (+ 1 test-counter))
       (let ((e form))
        (cond ((equal? obj e)
               (set! success-counter (+ 1 success-counter)))
              (else
                (set! failed-forms (cons 'form failed-forms)))))))))


(let ((p (open-input-string "abcd")))
 (check-equal #t (port? p))
 ;(check-equal #t (textual-port? p))
 ;(check-equal #f (binary-port? p))
 (check-equal "abcd" (read-string 4 p))
 (check-equal #t (eof-object? (read-char p)))
 (check-equal #t (eof-object? (peek-char p))))

(let ((p (open-input-string "one\ntwo\nthree")))
 (check-equal #\o (peek-char p))
 (check-equal "one" (read-line p))
 (check-equal "two" (read-line p))
 (check-equal "three" (read-line p))
 (check-equal #t (eof-object? (read-line p))))

(let ((p (open-input-string "one\ntwo\n")))
 ;; FIXME: How should the last line perform??
 (check-equal "one" (read-line p))
 (check-equal "two" (read-line p)))

(let ((p (open-input-bytevector (bytevector 1 2 3))))
 ;; FIXME: CSI does not return correct result here
 ;(check-equal 1 (peek-u8 p))
 (check-equal 1 (read-u8 p))
 (check-equal 2 (read-u8 p))
 (check-equal 3 (read-u8 p))
 ;(check-equal #t (eof-object? (peek-u8 p)))
 (check-equal #t (eof-object? (read-u8 p))))

(let ((p (open-input-bytevector (bytevector 1 2 3 4 5 6 7 8)))
      (buf (make-bytevector 3)))
  ;(check-equal #f (textual-port? p))
  ;(check-equal #t (binary-port? p))
  (check-equal (bytevector 1 2 3) (read-bytevector 3 p))
  (check-equal 3 (read-bytevector! buf p))
  (check-equal (bytevector 4 5 6) buf)
  (check-equal 2 (read-bytevector! buf p 1))
  (check-equal (bytevector 4 7 8) buf)
  ;(check-equal #t (eof-object? (peek-u8 p)))
  (check-equal #t (eof-object? (read-u8 p))))

(let ((p (open-output-string)))
 ;(check-equal #t (textual-port? p))
 ;(check-equal #f (binary-port? p))
 (write-char #\a p)
 (write-char #\b p)
 (write-char #\c p)
 (check-equal "abc" (get-output-string p)))

(let ((p (open-output-string)))
 (write-string "abcd" p 0 2)
 (write-string "abcd" p 3)
 (check-equal "abd" (get-output-string p)))

(let ((p (open-output-bytevector)))
 ;(check-equal #f (textual-port? p))
 ;(check-equal #t (binary-port? p))
 (write-u8 1 p)
 (write-u8 2 p)
 (write-u8 3 p)
 (check-equal (bytevector 1 2 3) (get-output-bytevector p)))

(let ((p (open-output-bytevector)))
 (write-bytevector (bytevector 1 2 3 4) p 0 2)
 (write-bytevector (bytevector 1 2 3 4) p 3)
 (check-equal (bytevector 1 2 4) (get-output-bytevector p)))

(let ((p (open-input-string "")))
 ;; FIXME: (read-string 0) case??
 (check-equal #t (eof-object? (read-string 1 p))))

(let ((p (open-input-string "abcd")))
 (check-equal "" (read-string 0 p)))


(check-finish)

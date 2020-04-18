;; I/O
(import (yuni scheme)
        (yunitest mini))

;;; File port

(define *testfiles*
  '("testfile.bin" "testfile.txt"))

(define (clear-file fil)
  (when (file-exists? fil)
    (delete-file fil)
    (when (file-exists? fil)
      (error "Testfile still there!" fil))) )

(define (testfiles-check)
  (for-each (lambda (fil) (when (file-exists? fil)
                            (error "(Previous?) testfile still exists!"
                                   fil)))
            *testfiles*))

(define (testfiles-remove)
  (for-each (lambda (fil) (clear-file fil))
            *testfiles*))

(testfiles-check)

;; Textual port

(clear-file "testfile.txt")
(let ((p (open-output-file "testfile.txt")))
 (check-equal #t (port? p))
 (check-equal #t (textual-port? p))
 (write-char #\h p)
 (write-char #\o p)
 (write-string "ge\n" p)
 (write-string "fuga" p)
 (close-port p))

(let ((p (open-input-file "testfile.txt")))
 (check-equal #t (port? p))
 (check-equal #t (textual-port? p))
 (check-equal #\h (peek-char p))
 (check-equal #\h (read-char p))
 (check-equal #\o (peek-char p))
 (check-equal #\o (read-char p))
 (check-equal "g" (read-string 1 p))
 (check-equal "e" (read-line p))
 (check-equal "fuga" (read-line p))
 (check-equal #t (eof-object? (read-line p)))
 (close-port p))

;; Binary port
(clear-file "testfile.bin")

(let ((p (open-binary-output-file "testfile.bin")))
 (check-equal #t (port? p))
 (check-equal #t (binary-port? p))
 (write-u8 1 p)
 (write-u8 2 p)
 (write-u8 3 p)
 (write-u8 4 p)
 (write-u8 5 p)
 (close-port p))

(let ((p (open-binary-input-file "testfile.bin"))
      (bv1 (make-bytevector 3 0))
      (bv2 (make-bytevector 3 0)))
 (check-equal #t (port? p))
 (check-equal #t (binary-port? p))
 (check-equal 3 (read-bytevector! bv1 p))
 (check-equal 2 (read-bytevector! bv2 p 0 3))
 (check-equal (bytevector 1 2 3) bv1)
 (check-equal (bytevector 4 5 0) bv2)
 (close-port p))

(testfiles-remove)

;;; Buffer port 


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

#| ;; FIXME: Gambit returns #!eof in this case (SIBR?)
(let ((p (open-input-string "abcd")))
 (check-equal "" (read-string 0 p)))
|#

(let ((p (open-input-string "abcd")))
 (check-equal "abcd" (read-string 99 p)))

(let ((p (open-output-string)))
 (let ((s1 (make-string 4097 #\c))
       (s2 (make-string 4099 #\c)))
   (write-string s1 p)
   (write-string s2 p)
   (check-equal (+ 4097 4099) (string-length (get-output-string p)))))

(let ((p (open-output-bytevector)))
 (let ((b1 (make-bytevector 4097))
       (b2 (make-bytevector 4099)))
   (write-bytevector b1 p)
   (write-bytevector b2 p)
   (check-equal (+ 4097 4099) (bytevector-length (get-output-bytevector p)))))

(check-finish)

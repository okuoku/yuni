(import (yuni scheme)
        (yuni miniread reader)
        (yunivm util simplerunner))



(define BUFSIZ 4096)

(define (source->bv pth)
  (define buf (make-bytevector BUFSIZ))
  (define in (open-binary-input-file pth))
  (define p (open-output-bytevector))
  (define (rd)
    (let ((r (read-bytevector! buf in)))
     (cond
       ((eof-object? r)
        (get-output-bytevector p))
       (else
         (write-bytevector buf p 0 r)
         (rd)))))
  (rd))

(define (file->sexp-list pth)
  (utf8-read (source->bv pth)))

(define (detectsource lis)
  (and (pair? lis)
       (or (and (string? (car lis))
                (string=? "-PROG" (car lis))
                (pair? (cdr lis))
                (cadr lis))
           (detectsource (cdr lis)))))


(define sourcefile (detectsource (command-line)))

(unless (and (string? sourcefile) 
             (file-exists? sourcefile))
  (error "file not found" sourcefile))

;; Run
(let ((src (file->sexp-list sourcefile)))
 (let ((r (new-simplerunner/fakeheap)))
  (let ((prog (simplerunner/expand-program r src)))
   (let ((ir (simplerunner/treeir-compile r prog)))
    ;(pp ir)
    (simplerunner/treeir-run r ir)))))

(display "Should not reach here.")

(exit 1)

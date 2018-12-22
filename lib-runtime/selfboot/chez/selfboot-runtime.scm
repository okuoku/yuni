;;
;; Runtime for selfboot
;;

(define (%%selfboot-load-aliaslib truename alias* export*)
  'do-nothing)

(define (%selfboot-file->sexp-list fn)
  (call-with-input-file 
    fn
    (lambda (p)
      (let loop ((cur '()))
       (let ((r (read p)))
        (if (eof-object? r)
          (reverse cur)
          (loop (cons r cur))))))))

(define %selfboot-file-exists? file-exists?)

(define (%selfboot-load prefix files)
  (for-each (lambda (e)
              (load (string-append %%selfboot-yuniroot "/" 
                                   prefix "/" e)))
            files))


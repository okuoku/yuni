;;
;; Runtime for selfboot
;;

(define-syntax library
  (syntax-rules ()
    ((_ name (export ...) (import ...) body ...)
     (begin body ...))))

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
                                   prefix "/" e)
                    %%myenv))
            files))


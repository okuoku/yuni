;;
;; FIXME: Remove the original selfboot-runtime.scm once yuniappjs updated
;;
;;
;; Runtime for selfboot
;;

(define (%%selfboot-load-aliaslib truename alias* export*)
  ;; Call library runtime
  (let ((lib (yuni/library-lookup truename)))
   (for-each (lambda (name)
               (yuni/library-add-alias! lib name))
             alias*)))

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

(define (%%selfboot-loadlib pth libname imports exports)
  (load pth))

(define (%selfboot-load prefix files)
  (for-each (lambda (e)
              (load (string-append %%selfboot-yuniroot "/" 
                                   prefix "/" e)))
            files))


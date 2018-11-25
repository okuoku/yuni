;;
;; Runtime for selfboot
;;

(define %%selfboot-fs (yuni/js-import "fs"))
(define %%selfboot-yuniroot (yuni/js-import "yuniroot"))

(define (%selfboot-file->sexp-list fn)
  (define (wrap-paren str)
    (string-append "(" str ")"))
  (read
    (open-input-string
      (wrap-paren
        (js-invoke %%selfboot-fs "readFileSync" fn "utf8")))))

(define %selfboot-file-exists? file-exists?)

(define (%selfboot-load prefix files)
  (for-each (lambda (e)
              (load (string-append %%selfboot-yuniroot "/" 
                                   prefix "/" e)))
            files))


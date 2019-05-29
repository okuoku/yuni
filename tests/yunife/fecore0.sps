(import (yuni scheme)
        (yunife core)
        (yunitest mini))

(define cmd (member "ROOT" (command-line)))
(define root (cadr cmd))

(define fe (make-yunife))

(define (path! pth)
  (yunife-add-path! fe (string-append root "/" pth)))

;; FIXME: Read config/config.scm ?
(yunife-add-alias-map! fe 'yuni 'yunife-yuni)
(yunife-add-alias-map! fe 'yunivm 'yunife-yunivm)
(for-each path!
          (list
            "lib"
            "lib-compat"
            "lib-r7c"
            "lib-r6rs"
            "external"))


(yunife-load-sexp-list! fe
                        '((import (yuni scheme)
                                  (yunitest mini))))

(check-finish)

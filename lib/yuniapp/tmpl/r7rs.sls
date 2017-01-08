(library (yuniapp tmpl r7rs)
         (export tmpl-r7rs/chibi-scheme
                 tmpl-r7rs/gauche)
         (import (yuni scheme)
                 (yuniapp util tmplutils))

;;

(define (%stripdirs pth)
  ;; Strip rootpath for library file
  ;; Because chibi-scheme always appends library path to include file
  (define (itr acc cur)
    (if (pair? cur)
      (let ((c (car cur)))
       (if (or (char=? #\/ c) (char=? #\\ c))
         (list->string acc)
         (itr (cons c acc) (cdr cur))))
      (list->string acc)))
  (itr '() (reverse (string->list pth))))

(define (tmpl-r7rs/gauche libname exportsyms importset pth)
  (string-append
    "(define-library " (libname->string libname) "\n"
    "    (export\n" (exportsyms->string exportsyms)
    ")\n"
    "    (import \n"
    "      (yuni-runtime r7rs)\n"
    (sexp-list->lines importset)
    ")\n"
    "(include \"" (escape-path pth) "\"))\n"))
         
(define (tmpl-r7rs/chibi-scheme libname exportsyms importset pth)
  (string-append
    "(define-library " (libname->string libname) "\n"
    "    (export\n" (exportsyms->string exportsyms)
    ")\n"
    "    (import \n"
    "      (yuni-runtime r7rs)\n"
    (sexp-list->lines importset)
    ")\n"
    "(include \"" (escape-path (%stripdirs pth)) "\"))\n"))
         
)

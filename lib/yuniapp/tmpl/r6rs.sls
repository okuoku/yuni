(library (yuniapp tmpl r6rs)
         (export tmpl-r6rs/racket
                 tmpl-r6rs/guile)
         (import (yuni scheme)
                 (yuniapp util tmplutils))

;;

(define (tmpl-r6rs/guile libname exportsyms importset pth)
  (string-append
    "#!r6rs\n"
    "(library " (libname->string libname) "\n"
    "    (export\n" (exportsyms->string exportsyms)
    ")\n"
    "    (import \n"
    "      (yuni-runtime guile)\n"
    "      (rename (only (guile) include)\n"
    "           (include %%internal-paste:include))\n"
    (sexp-list->lines importset)
    ")\n"
    "(%%internal-paste:include \"" (escape-path pth) "\"))\n"))
         
(define (tmpl-r6rs/racket libname exportsyms importset pth)
  (string-append
    "#!r6rs\n"
    "(library " (libname->string libname) "\n"
    "    (export\n" (exportsyms->string exportsyms)
    ")\n"
    "    (import \n"
    "      (yuni-runtime racket)\n"
    "      (only (racket) file)\n"
    "      (rename (only (racket include) include)\n"
    "           (include %%internal-paste:include))\n"
    (sexp-list->lines importset)
    ")\n"
    "(%%internal-paste:include (file \"" (escape-path pth) "\")))\n"))
         
)

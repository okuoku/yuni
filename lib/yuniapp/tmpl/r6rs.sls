(library (yuniapp tmpl r6rs)
         (export tmpl-r6rs/racket
                 ; FIXME: Currently we do not need alias variant
                 ;tmpl-r6rs-alias/racket
                 )
         (import (yuni scheme)
                 (yuniapp util tmplutils))

;;

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
    "(%%internal-paste:include (file \"" pth "\")))\n"))
         
)

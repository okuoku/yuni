(import (yuni scheme)
        (yuni util files)
        (yuni compat ident))

(define myname (symbol->string (ident-impl)))

(when (file-exists? myname)
  (error "File exists..."))

(create-directory myname)

(unless (or (eq? (ident-impl) 'ironscheme) 
            (eq? (ident-impl) 'kawa)
            (file-directory? myname))
  (error "Not a directory"))

(delete-directory myname)

(when (file-exists? myname)
  (error "File exists..."))

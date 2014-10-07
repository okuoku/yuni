(library (racket-r7b define-values)
         (export define-values)
         ;; FIXME: Racket define-values do not support (define-values x ...)
         ;;        form.
         (import (r7b-util define-values)))

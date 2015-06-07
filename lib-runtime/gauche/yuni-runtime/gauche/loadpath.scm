(define-module yuni-runtime.gauche.loadpath
               (export loadpath))

(select-module yuni-runtime.gauche.loadpath)

(define (loadpath) *load-path*)

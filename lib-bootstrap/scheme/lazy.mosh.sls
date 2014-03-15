;; this file is an alias-library.
;;  alias of:
;;   lib/r7b-impl/lazy.sls
(library (scheme lazy)
         (export
             promise?
             force
             delay
             make-promise
             delay-force
         )
         (import
             (r7b-impl lazy)
         )
) ;; library (scheme lazy)

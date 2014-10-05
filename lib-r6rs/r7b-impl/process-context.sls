(library (r7b-impl process-context)
         (export
;; from R7RS draft 7
command-line emergency-exit exit 
get-environment-variable get-environment-variables
)
         (import (rnrs) (r7b-compat i98) (r7b-util emergency-exit)))

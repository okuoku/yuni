(library (gambit-compat-scheme process-context)
         (export
           command-line emergency-exit
           exit
           get-environment-variable
           get-environment-variables
           )
         (import)

(define-primitive-names/yunifake
  command-line emergency-exit
  exit
  get-environment-variables)

(define get-environment-variable 'YUNIFAKE-UNIMPLEMENTED)
         
)

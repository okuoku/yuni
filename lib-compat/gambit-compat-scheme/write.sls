(library (gambit-compat-scheme write)
         (export 
           display write write-shared write-simple )
         (import)

(define-primitive-names/yunifake
  display write write-shared)

(define write-simple 'YUNIFAKE-UNIMPLEMENTED)
         
)

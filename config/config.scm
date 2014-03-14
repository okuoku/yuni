;; Library directories which require import-stubs for R7RSs and Racket
(*library-directories*
  "lib"
  "lib-r6rs"
  "lib-compat")

(*library-groups*
  (yuni yuni)
  ;; R7RS-bridge: R7RS library for R6RS implementations
  (r7rs-bridge
    r7b-util
    (r7b-impl => scheme))
  (compat-racket
    (racket-yuni => yuni)
    (racket-srfi => srfi)))

(GenRacket
  ;; Racket library generator
  (racket
    yuni
    r7rs-bridge
    compat-racket
    )) 

(GenR7RS
  ;; R7RS library generator
  (gauche
    yuni) 
  (chibi
    yuni)) 

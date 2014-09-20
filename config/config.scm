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
  (compat-chibi
    (chibi-yuni => yuni))
  (compat-gauche
    (gauche-yuni => yuni))
  (compat-racket
    (racket-yuni => yuni)
    (racket-srfi => srfi))
  (r7rs-common
    (r7rs-common-yuni => yuni))
  (r6rs-common
    (r6rs-common-yuni => yuni)))

(GenRacket
  ;; Racket library generator
  (racket
    yuni
    r7rs-bridge
    r6rs-common
    compat-racket
    )) 

(GenR7RS
  ;; R7RS library generator
  (gauche
    r7rs-common
    compat-gauche
    yuni) 
  (chibi
    r7rs-common
    compat-chibi
    yuni)) 

(GenR6RSCommon
  ;; R6RS-common can import (yuni ...) libraries directly
  (r6rs-common
    r6rs-common)
  (guile
    r7rs-bridge
    r6rs-common)
  )

;; Library directories
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
  (compat-guile
    (guile-srfi => srfi))
  (compat-sagittarius
    (sagittarius-yuni => yuni))
  (compat-racket
    (racket-yuni => yuni)
    (racket-srfi => srfi))
  (compat-picrin
    (picrin-yuni => yuni))
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
    yuni)
  (picrin
    r7rs-common
    compat-picrin
    yuni)
  ;; Sagittarius requires keyword syntax shim.
  (sagittarius
    yuni
    r6rs-common
    compat-sagittarius)
  ) 

(GenR6RSCommon
  ;; R6RS-common assumes target can import R6RS-light libraries directly
  ;; Thus, it just renames the libraries
  (r6rs-common
    r6rs-common)
  ;; Guile can import R6RS-light but no R7RS
  (guile
    compat-guile
    r7rs-bridge
    r6rs-common)
  )

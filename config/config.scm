;; Library directories
(*library-directories*
  "lib"
  "lib-r6rs"
  "lib-compat")

(*library-groups*
  (yuni yuni yunisrfi)
  ;; R7RS-bridge: R7RS library for R6RS implementations
  (r7rs-bridge
    r7b-util
    (r7b-impl => scheme))
  (compat-chicken
    (chicken-yuni => yuni))
  (compat-chibi
    (chibi-yuni => yuni))
  (compat-gauche
    (gauche-yuni => yuni))
  (compat-guile
    (guile-srfi => r7b-compat))
  (compat-sagittarius
    (sagittarius-yuni => yuni))
  (compat-racket
    (racket-yuni => yuni)
    (racket-srfi => r7b-compat))
  (compat-chez
    (chez-srfi => r7b-compat))
  (compat-picrin
    (picrin-yuni => yuni))
  (compat-ironscheme
    (ironscheme-srfi => r7b-compat))
  (r7rs-common
    (r7rs-common-yuni => yuni))
  (r6rs-common
    (r6rs-common-yuni => yuni)))

(GenRacket
  ;; Racket library generator
  (racket
    yuni
    yunisrfi
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
  (chicken
    r7rs-common
    compat-chicken
    yuni)
  ;; Sagittarius requires keyword syntax shim.
  (sagittarius
    yuni
    r7rs-common
    compat-sagittarius)) 

(GenR6RSCommon
  ;; R6RS-common assumes target can import R6RS-light libraries directly
  ;; Thus, it just renames the libraries
  (r6rs-common
    r6rs-common)
  (ironscheme
    compat-ironscheme
    r6rs-common
    r7rs-bridge)
  ;; Guile can import R6RS-light but no R7RS
  (guile
    compat-guile
    r7rs-bridge
    r6rs-common)
  (chez
    compat-chez
    r7rs-bridge
    r6rs-common)
  )

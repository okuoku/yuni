;; Library directories
(*library-directories*
  "lib"
  "lib-r6rs"
  "lib-compat")

(*library-groups*
  ;; Yuni
  (yuni yuni yunisrfi)
  ;; R7RS
  (compat-chicken
    (chicken-yuni => yuni)) 
  (compat-chibi
    (chibi-yuni => yuni))
  (compat-gauche
    (gauche-yuni => yuni))
  (compat-sagittarius
    (sagittarius-yuni => yuni))
  (compat-picrin
    (picrin-yuni => yuni))
  (compat-kawa
    (kawa-yuni => yuni))
  (r7rs-common
    (r7rs-common-yuni => yuni))
  ;; R6RS
  ;;; R7RS-bridge: R7RS library for R6RS implementations
  (r7rs-bridge ;; Some R6RS might not need this tough
    r7b-util
    (r7b-impl => scheme))
  (compat-guile
    (guile-yuni => yuni)
    (guile-r7b => r7b-compat))
  (compat-racket
    (racket-yuni => yuni)
    (racket-r7b => r7b-compat))
  (compat-chez
    (chez-r7b => r7b-compat))
  (compat-ironscheme
    (ironscheme-r7b => r7b-compat))
  (compat-nmosh
    (nmosh-yuni => yuni)
    (nmosh-r7b => r7b-compat))
  (compat-larceny
    (larceny-yuni => yuni))
  (compat-vicare
    (vicare-r7b => r7b-compat)
    (vicare-yuni => yuni))
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
  (kawa
    yuni
    r7rs-common
    compat-kawa)
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
  ;; Larceny has R7RS libraries
  (larceny
    compat-larceny
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
  (vicare
    compat-vicare
    r7rs-bridge
    r6rs-common)
  ;; Of course, we should do some dog-food
  (nmosh
    compat-nmosh
    r7rs-bridge
    r6rs-common)
  )

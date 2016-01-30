;; Library directories
(*library-directories*
  "lib-stub/gen"
  "lib"
  "lib-r6rs"
  "lib-compat")

(*library-groups*
  ;; Yuni
  (yuni yuni yunisrfi yuniffi)
  (yuni-r6rs yuni-r6rs)
  ;; FIXME: Hack. We don't have any convention for generated libraries yet.
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
    (nmosh-r7b => r7b-compat)
    (nmosh-r7b-util => r7b-util))
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
    yuni-r6rs
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
    yuni-r6rs
    compat-sagittarius)) 

(GenR6RSCommon
  ;; R6RS-common assumes target can import R6RS-light libraries directly
  ;; Thus, it just renames the libraries
  (r6rs-common
    r6rs-common)
  ;; Larceny has R7RS libraries
  (larceny
    yuni-r6rs
    compat-larceny
    r6rs-common)
  (ironscheme
    yuni-r6rs
    compat-ironscheme
    r6rs-common
    r7rs-bridge)
  ;; Guile can import R6RS-light but no R7RS
  (guile
    yuni-r6rs
    compat-guile
    r7rs-bridge
    r6rs-common)
  (chez
    yuni-r6rs
    compat-chez
    r7rs-bridge
    r6rs-common)
  (vicare
    yuni-r6rs
    compat-vicare
    r7rs-bridge
    r6rs-common)
  ;; Of course, we should do some dog-food
  (nmosh
    yuni-r6rs
    compat-nmosh
    r7rs-bridge
    r6rs-common)
  )

;; Library directories
(*library-directories*
  "lib"
  "lib-r7c"
  "lib-r6rs"
  "lib-compat"
  "external")

(*library-groups*
  ;; Yuni
  (yuni yuni yuniffi yuniconfig yuniapp yunivm yuniexternal
        ;; YuniVM Runtimes
        yunivmrt r7c-basic r7c-equiv r7c-numeric r7c-report r7c-system
        r7c-io
        r7c-yunicore)
  (yunifake yunifake-util)
  (r7rs-common
    (r7rs-common-yuni => yuni))
  (r6rs-common
    (r6rs-common-yuni => yuni))
  ;;; R7RS-bridge: R7RS library for R6RS implementations
  (r7rs-bridge ;; Some R6RS might not need this tough
    (r7b-yuni => yuni) ;; Yuni scheme standard libraries
    r7b-util)
  (r7b-r7rs-libs ;; Racket may have incompatible scheme/base library
    (r7b-impl => scheme))
  (r7b-r7rs-libs/racket
    r7b-impl)
  ;; R6RS+R7RS
  (compat-digamma
    (digamma-yuni => yuni))
  (compat-guile3 ;; Guile3 do not use Yuni's own R7RS layer
    (guile-yuni => yuni))
  (compat-sagittarius
    (sagittarius-yuni => yuni))
  
  ;; R7RS
  (compat-chibi-scheme
    (chibi-scheme-yuni => yuni))
  (compat-chicken
    (chicken-yuni => yuni)) 
  (compat-cyclone
    (cyclone-yuni => yuni))
  (compat-gauche
    (gauche-yuni => yuni))
  (compat-kawa
    (kawa-yuni => yuni))
  (compat-foment
    (foment-yuni => yuni))

  ;; R6RS
  (compat-chez
    (chez-yuni => yuni)
    (chez-r7b => r7b-compat)
    (chez-r7b-util => r7b-util))
  (compat-guile ;; Guile 2.x
    (guile-yuni => yuni)
    (guile-r7b => r7b-compat))
  (compat-ironscheme
    (ironscheme-yuni => yuni)
    (ironscheme-r7b => r7b-compat))
  (compat-racket
    (racket-yuni => yuni)
    (racket-r7b => r7b-compat))

  ;; Generic Scheme
  (compat-biwascheme
    ;; Must sync. with *yuni/libalias*
    (biwascheme-yuni => yuni)
    (biwascheme-compat-scheme => scheme))
  (compat-gambit
    (gambit-yuni => yuni)
    (gambit-compat-scheme => scheme))
  (compat-mit-scheme
    (mit-scheme-yuni => yuni)
    (mit-scheme-compat-scheme => scheme))
  (compat-s7
    ;; Must sync. with *yuni/libalias*
    (s7-yuni => yuni)
    (s7-scheme => scheme))
  (compat-scm
    (scm-yuni => yuni)
    (scm-scheme => scheme))
  (compat-stklos
    (stklos-yuni => yuni)
    (stklos-scheme => scheme))
  (compat-bigloo
    (bigloo-yuni => yuni)
    (bigloo-scheme => scheme))

  ;; Unsupported
  (compat-picrin
    (picrin-yuni => yuni))
  (compat-nmosh
    (nmosh-yuni => yuni)
    (nmosh-r7b => r7b-compat)
    (nmosh-r7b-util => r7b-util))
  (compat-larceny
    (larceny-yuni => yuni))
  (compat-vicare
    (vicare-r7b => r7b-compat)
    (vicare-yuni => yuni))
  (compat-rapid-gambit
    (rapid-gambit-yuni => yuni))
  )

(GenRacket
  ;; Racket-styled library generator
  (racket
    r7b-r7rs-libs/racket
    yuni
    ;yunisrfi
    r7rs-bridge
    r6rs-common
    compat-racket
    )
  ;; Guile can import R6RS-light but no R7RS
  (guile
    r7b-r7rs-libs
    yuni
    ;yunisrfi
    compat-guile
    r7rs-bridge
    r6rs-common)) 

(GenR7RS
  ;; R7RS library generator
  (gauche
    compat-gauche
    r7rs-common
    yuni) 
  (chibi-scheme
    compat-chibi-scheme
    r7rs-common
    yuni)
  (picrin
    compat-picrin
    r7rs-common
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
    compat-sagittarius
    yuni
    r7rs-common
    r6rs-common)
  ;; experimental
  (rapid-gambit
    yuni
    r7rs-common
    compat-rapid-gambit)
  (cyclone
    yuni
    r7rs-common
    compat-cyclone)
  (foment
    yuni
    r7rs-common
    compat-foment)
  ) 

(GenR6RSCommon
  ;; R6RS-common assumes target can import R6RS-light libraries directly
  ;; Thus, it just renames the libraries
  ;; Larceny has R7RS libraries
  (larceny
    yuni
    compat-larceny
    r6rs-common
    r7rs-common)
  (ironscheme
    r7b-r7rs-libs
    yuni
    compat-ironscheme
    r6rs-common
    r7rs-bridge)
  (chez
    r7b-r7rs-libs
    yuni
    compat-chez
    r7rs-bridge
    r6rs-common)
  (vicare
    r7b-r7rs-libs
    yuni
    compat-vicare
    r7rs-bridge
    r6rs-common)
  ;; Of course, we should do some dog-food
  (nmosh
    r7b-r7rs-libs
    yuni
    compat-nmosh
    r7rs-bridge
    r6rs-common)
  ;; Experimental Gambit R5RS
  (gambit
    yuni
    r7rs-common
    compat-gambit
    yunifake)
  ;; Experimental MIT/GNU Scheme R5RS
  (mit-scheme
    yuni
    yunifake
    r7rs-common
    compat-mit-scheme)
  (s7
    yuni
    r7rs-common
    compat-s7)
  (biwascheme
    yuni
    r7rs-common
    compat-biwascheme)
  (guile3
    compat-guile3
    yuni
    r6rs-common
    r7rs-common)
  (digamma
    compat-digamma
    yuni
    r6rs-common
    r7rs-common)
  (stklos
    yuni
    yunifake
    r7rs-common
    compat-stklos)
  (scm
    yuni
    yunifake
    r7rs-common
    compat-scm)
  (bigloo
    yuni
    yunifake
    r7rs-common
    compat-bigloo)
  )

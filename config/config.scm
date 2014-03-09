(*library-directories*
  "lib"
  "lib-r6rs")

(*library-groups*
  (yuni yuni)
  (r7rs-bridge
    r7b-util
    (r7b-impl => scheme)))

(GenRacket
  ;; Racket library generator
  (racket
    yuni
    r7rs-bridge)) 

(GenR7RS
  ;; R7RS library generator
  (gauche
    yuni) 
  (chibi
    yuni)) 

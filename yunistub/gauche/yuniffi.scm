(define-module yuniffi
  (export yuniffi-nccc-call
          yuniffi-nccc-bootstrap
          yuniffi-pointer-fetch-signed
          yuniffi-pointer-fetch-unsigned
          yuniffi-pointer-store
          yuniffi-pointer-fromint
          yuniffi-pointer-fetch-p64
          yuniffi-pointer-store-p64
          yuniffi-pointer-fetch-p64/bv
          yuniffi-pointer-store-p64/bv
          yuniptr?))
(select-module yuniffi)

(dynamic-load "yuniffi")

(define (yuniptr? x) (is-a? x <yuniptr>))


(library (yuni ffi abi abiv0-runtime)
         (export
           yuniffi-abiv0-lookup/constants
           yuniffi-abiv0-lookup/bridgestubs
           yuniffi-abiv0-get-table)
         (import (yuni scheme)
                 (yuni ffi abi abiv0)
                 (yuni compat bitwise primitives)
                 (yuni compat ffi primitives))

;; 

(define (constants-name x)
  (string-append x yuniffi-abiv0-export-suffix/constants))
(define (bridgestubs-name x)
  (string-append x yuniffi-abiv0-export-suffix/bridgestubs))

(define (lookupbody mod name)
  (yuniffi-module-lookup mod name))

(define (yuniffi-abiv0-lookup/constants module name) ;; => func / #f
  (lookupbody module (constants-name name)))

(define (yuniffi-abiv0-lookup/bridgestubs module name)
  (lookupbody module (bridgestubs-name name)))

(define (getrow func idx) ;; => ("name" flags value size offset)
  (define in (make-bytevector (* 8 2) 0))
  (define out (make-bytevector (* 8 8)))

  ;; Setup in packet
  (bv-write/u64! in 8 idx)

  ;; Query
  (yuniffi-nccc-call func in 0 2 out 0 8)

  ;; Check for valid bit
  (let ((out0 (bv-read/u64 out 0))
        (out1p (bv-read/w64ptr out (* 8 1)))
        (out2 (bv-read/u64 out (* 8 2)))
        (out3 (bv-read/u64 out (* 8 3)))
        (out4 (bv-read/u64 out (* 8 4)))
        (out5 (bv-read/u64 out (* 8 5))))
   (and (not (= out0 0))
        (list 
          (and
            (not (= out0 YUNIFFI_SYMBOL__TERMINATE))
            (ptr-read/asciiz out1p 0 (+ out2 1)))
          out0
          out3
          out4
          out5))))

(define (yuniffi-abiv0-get-table tbl)
  (define (itr idx cur)
    (define row (getrow tbl idx))
    (if row
      (let ((flags (cadr row)))
       (if (= YUNIFFI_SYMBOL__TERMINATE
              flags)
         (reverse cur)
         (itr (+ 1 idx) (cons row cur))) )
      (itr (+ 1 idx) cur)))
  (itr 1 '()))

)

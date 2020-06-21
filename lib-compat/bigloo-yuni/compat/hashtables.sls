(library (bigloo-yuni compat hashtables)
         (export
           ;; Yuni extension
           ; Constructors
           make-integer-hashtable
           make-string-hashtable
           make-symbol-hashtable
           ; Iter
           hashtable-for-each ;; Bigloo
           hashtable-fold

           ;; R6RS
           ; Constructor
           make-eq-hashtable
           make-eqv-hashtable
           ; Mutators
           hashtable-set!
           hashtable-update! ;; Bigloo
           ; Accessor
           hashtable-entries
           hashtable-ref
           ; Query
           hashtable-keys
           hashtable-size ;; Bigloo
           )
         (import (yuni scheme))

;; Implemented in prelib
(error "This library should not be loaded")

)

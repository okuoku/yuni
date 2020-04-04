(library (scm-yuni compat hashtables)
         (export
           ;; Yuni extensions
           make-symbol-hashtable
           ;; 13.1 Constructors
           make-eq-hashtable
           make-eqv-hashtable
           make-integer-hashtable
           make-string-hashtable
           make-hashtable

           ;; 13.2 Procedures
           hashtable?
           hashtable-size
           hashtable-ref
           hashtable-set!
           hashtable-delete!
           ;hashtable-contains?
           ;hashtable-update!
           ;hashtable-copy
           ;hashtable-clear!
           hashtable-keys
           hashtable-entries

           ;; 13.3 Inspection
           ;hashtable-equivalence-function
           ;hashtable-hash-function
           ;hashtable-mutable?

           ;; 13.4 Hash functions
           ;equal-hash
           ;string-hash
           ;string-ci-hash
           ;symbol-hash

           hashtable-for-each
           hashtable-fold
           )
         (import (yuni scheme))
)

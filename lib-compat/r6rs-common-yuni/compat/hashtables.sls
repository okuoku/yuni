(library (r6rs-common-yuni compat hashtables)
         (export
           ;; 13.1 Constructors
           make-eq-hashtable
           make-eqv-hashtable
           make-hashtable

           ;; 13.2 Procedures
           hashtable?
           hashtable-size
           hashtable-ref
           hashtable-set!
           hashtable-delete!
           hashtable-contains?
           hashtable-update!
           hashtable-copy
           hashtable-clear!
           hashtable-keys
           hashtable-entries

           ;; 13.3 Inspection
           hashtable-equivalence-function
           hashtable-hash-function
           hashtable-mutable?

           ;; 13.4 Hash functions
           equal-hash
           string-hash
           string-ci-hash
           symbol-hash
           )
         (import (rnrs hashtables)))

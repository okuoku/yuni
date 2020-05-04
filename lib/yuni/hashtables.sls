(library (yuni hashtables)
         (export
           ;; Yuni extension
           ; Constructors
           make-integer-hashtable
           make-string-hashtable
           make-symbol-hashtable
           ; Iter
           hashtable-for-each
           hashtable-fold

           ;; R6RS
           ; Constructor
           make-eq-hashtable
           make-eqv-hashtable
           ; Mutators
           hashtable-set!
           hashtable-update!
           ; Accessor
           hashtable-entries
           hashtable-ref
           ; Query
           hashtable-keys
           hashtable-size)
         (import (yuni compat hashtables)))

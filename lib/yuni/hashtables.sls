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
           ; Mutators
           hashtable-set!
           hashtable-update!
           hashtable-entries
           ; Accessor
           hashtable-ref
           ; Query
           hashtable-size)
         (import (yuni compat hashtables)))

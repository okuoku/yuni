(library (yuni ffi abi abiv0)
         (export
           yuniffi-abiv0-export-suffix/constants
           yuniffi-abiv0-export-suffix/bridgestubs
           YUNIFFI_SYMBOL__TERMINATE
           YUNIFFI_SYMBOL__VALID)
         (import (yuni scheme))

(define yuniffi-abiv0-export-suffix/constants
  "_export_constants")
(define yuniffi-abiv0-export-suffix/bridgestubs
  "_export_bridgestubs")

(define YUNIFFI_SYMBOL__TERMINATE 1)
(define YUNIFFI_SYMBOL__VALID 2)
(define YUNIFFI_SYMBOL__HAS_SIZE 4)
(define YUNIFFI_SYMBOL__HAS_OFFSET 8)
         
)

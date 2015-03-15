(c-include "yuniffi_stub.h")

(define-c (pointer void) yuniffi_nccc_bootstrap ())
(define-c void yuniffi_nccc_call 
          ((pointer void)
            sexp int int 
            sexp int int))

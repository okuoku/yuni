(c-include "yuniffi_stub.h")

(define-c (pointer void) yuniffi_nccc_bootstrap ())
#|
(define-c void yuniffi_nccc_call 
          ((pointer void)
            sexp int int 
            sexp int int))
|#

(define-c (pointer void) yuniffi_offset_ptr ((maybe-null pointer void) sexp))
(define-c int yuniffi_pointerp (sexp))
(define-c int yuniffi_fetch_s8 ((pointer void) int))
(define-c int yuniffi_fetch_u8 ((pointer void) int))
(define-c int yuniffi_fetch_s16 ((pointer void) int))
(define-c int yuniffi_fetch_u16 ((pointer void) int))
(define-c int yuniffi_fetch_s32 ((pointer void) int))
(define-c unsigned-int yuniffi_fetch_u32 ((pointer void) int))

(define-c void yuniffi_store_s8 ((pointer void) int int))
(define-c void yuniffi_store_u8 ((pointer void) int unsigned-int))
(define-c void yuniffi_store_s16 ((pointer void) int int))
(define-c void yuniffi_store_u16 ((pointer void) int unsigned-int))
(define-c void yuniffi_store_s32 ((pointer void) int int))
(define-c void yuniffi_store_u32 ((pointer void) int unsigned-int))

(define-c (pointer void) yuniffi_fetch_p64 ((pointer void) int))
(define-c void yuniffi_store_p64 ((pointer void) int (pointer void)))
(define-c (pointer void) yuniffi_fetch_p64_bv (sexp int))
(define-c void yuniffi_store_p64_bv (sexp int (pointer void)))

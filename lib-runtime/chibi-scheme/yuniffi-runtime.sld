(define-library (yuniffi-runtime)
                (export yuniffi_nccc_call
                        yuniffi_nccc_bootstrap
                        yuniffi_offset_ptr
                        yuniffi_pointerp
                        yuniffi_fetch_s8
                        yuniffi_fetch_u8
                        yuniffi_fetch_s16
                        yuniffi_fetch_u16
                        yuniffi_fetch_s32
                        yuniffi_fetch_u32
                        yuniffi_store_s8
                        yuniffi_store_u8
                        yuniffi_store_s16
                        yuniffi_store_u16
                        yuniffi_store_s32
                        yuniffi_store_u32
                        yuniffi_fetch_p64
                        yuniffi_store_p64
                        yuniffi_fetch_p64_bv
                        yuniffi_store_p64_bv
                        )
                (include-shared "chibi-scheme-yuniffi"))

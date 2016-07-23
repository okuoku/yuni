(library (chicken-yuni compat ffi primitives)
         (export
           ;yuniffi-module-load
           ;yuniffi-module-lookup

           #|
           ;; Memory OPs (pointers)
           ptr? integer->ptr
           ptr-read/s8 ptr-read/u8 ptr-read/s16 ptr-read/u16
           ptr-read/s32 ptr-read/u32 ptr-read/s64 ptr-read/u64
           ptr-read/asciiz
           ptr-write/s8! ptr-write/u8! ptr-write/s16! ptr-write/u16!
           ptr-write/s32! ptr-write/u32! ptr-write/s64! ptr-write/u64!
           ptr-write/asciiz!
           |#
           yuniffi-nccc-call
           
           )

         (import
           (yuni scheme)
           #|
           (yuni ffi runtime bootstraploader)
           (yuni ffi runtime simpleloader)
           (yuni ffi runtime simplestrings)
           |#
           (yuniffi-chicken)
           (lolevel))

(define (yuniffi-nccc-call func in in-offset in-len out out-offset out-len)
  (%%yuniffi-nccc-call func in in-offset in-len out out-offset out-len))
         
)

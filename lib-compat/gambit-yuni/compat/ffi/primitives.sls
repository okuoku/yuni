(library (gambit-yuni compat ffi primitives)
         (export
           yuniffi-nccc-call
           yuniffi-module-load
           yuniffi-module-lookup

           ;; Memory OPs (pointers)
           ptr? 
           ptr-read/w64ptr
           ptr-write/w64ptr!
           ptr-read/s8 ptr-read/u8 ptr-read/s16 ptr-read/u16
           ptr-read/s32 ptr-read/u32 ptr-read/s64 ptr-read/u64
           ptr-read/asciiz
           ptr-write/s8! ptr-write/u8! ptr-write/s16! ptr-write/u16!
           ptr-write/s32! ptr-write/u32! ptr-write/s64! ptr-write/u64!
           ptr-write/asciiz!

           bv-read/w64ptr
           bv-write/w64ptr!
           )
         (import 
           (gambit-yuni compat ffi primitives-phase0)
           (gambit-yuni compat ffi primitives-phase1)
           (gambit-yuni compat ffi primitives-phase2))

)

(library (vicare-yuni compat bitwise primitives)
         (export 
           ;; Memory OPs (bytevectors)
           bv-read/s8 bv-read/u8 bv-read/s16 bv-read/u16
           bv-read/s32 bv-read/u32 bv-read/s64 bv-read/u64
           bv-read/f32 bv-read/f64
           bv-read/asciiz
           bv-write/s8!  bv-write/u8!  bv-write/s16!  bv-write/u16!
           bv-write/s32! bv-write/u32!  bv-write/s64!  bv-write/u64!
           bv-write/f32! bv-write/f64!
           bv-write/asciiz!)
         (import (yuni-r6rs ffi memory)))

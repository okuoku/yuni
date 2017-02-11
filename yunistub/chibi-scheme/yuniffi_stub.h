/* yuniffi_stub */

#ifndef YUNIFFI_STUB_H
#define YUNIFFI_STUB_H

#include <chibi/eval.h>

void yuniffi_nccc_call(sexp ctx, void* func, 
                       sexp in, int in_offset, int in_len,
                       sexp out, int out_offset, int out_len);
void* yuniffi_nccc_bootstrap(void);

int yuniffi_pointerp(sexp obj);
void* yuniffi_offset_ptr(void* ptr, sexp offset);
int yuniffi_fetch_s8(void* ptr, int offset);
int yuniffi_fetch_u8(void* ptr, int offset);
int yuniffi_fetch_s16(void* ptr, int offset);
int yuniffi_fetch_u16(void* ptr, int offset);
/* POSIX requires 32bit int */
int yuniffi_fetch_s32(void* ptr, int offset);
unsigned int yuniffi_fetch_u32(void* ptr, int offset);
/* 64bit values would be boxed */
sexp yuniffi_fetch_s64(void* ptr, int offset);
sexp yuniffi_fetch_u64(void* ptr, int offset);

void yuniffi_store_s8(void* ptr, int offset, int value);
void yuniffi_store_u8(void* ptr, int offset, unsigned int value);
void yuniffi_store_s16(void* ptr, int offset, int value);
void yuniffi_store_u16(void* ptr, int offset, unsigned int value);
void yuniffi_store_s32(void* ptr, int offset, int value);
void yuniffi_store_u32(void* ptr, int offset, unsigned int value);
void yuniffi_store_s64(void* ptr, int offset, sexp value);
void yuniffi_store_u64(void* ptr, int offset, sexp value);

/* 64bit pointer handling */
void* yuniffi_fetch_p64(void* ptr, int offset);
void* yuniffi_fetch_p64_bv(sexp bv, int offset);
void yuniffi_store_p64(void* ptr, int offset, void* value);
void yuniffi_store_p64_bv(sexp bv, int offset, void* value);

#endif

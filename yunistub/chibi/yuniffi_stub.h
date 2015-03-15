/* yuniffi_stub */

#ifndef YUNIFFI_STUB_H
#define YUNIFFI_STUB_H

#include <chibi/eval.h>

void yuniffi_nccc_call(void* func, 
                       sexp in, int in_offset, int in_len,
                       sexp out, int out_offset, int out_len);
void* yuniffi_nccc_bootstrap(void);

#endif

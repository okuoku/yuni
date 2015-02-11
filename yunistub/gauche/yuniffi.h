/*
 * yuniffi.h
 */

/* Prologue */
#ifndef GAUCHE_YUNIFFI_H
#define GAUCHE_YUNIFFI_H

#include <gauche.h>
#include <gauche/extend.h>

SCM_DECL_BEGIN

extern ScmObj yuniffi_nccc_call(ScmObj func,
                                ScmObj in, ScmObj in_offset, ScmObj in_len,
                                ScmObj out, ScmObj out_offset, ScmObj out_len);

extern ScmObj yuniffi_nccc_bootstrap(void);

/* Epilogue */
SCM_DECL_END

#endif  /* GAUCHE_YUNIFFI_H */

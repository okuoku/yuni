/*
 * yuniffi.h
 */

/* Prologue */
#ifndef GAUCHE_YUNIFFI_H
#define GAUCHE_YUNIFFI_H

#include <gauche.h>
#include <gauche/extend.h>

SCM_DECL_BEGIN

extern ScmClass* YuniPtrCls;

#define YUNIPTR_UNBOX(obj) SCM_FOREIGN_POINTER_REF(void*, obj)
#define YUNIPTR_P(obj) SCM_XTYPEP(obj, YuniPtrCls)
#define YUNIPTR_BOX(ptr) Scm_MakeForeignPointer(YuniPtrCls, ptr)

extern ScmObj yuniffi_nccc_call(ScmObj func,
                                ScmObj in, ScmObj in_offset, ScmObj in_len,
                                ScmObj out, ScmObj out_offset, ScmObj out_len);

extern ScmObj yuniffi_nccc_bootstrap(void);

extern ScmObj yuniffi_pointer_fetch_signed(ScmObj ptr, ScmObj offset, 
                                           ScmObj width);
extern ScmObj yuniffi_pointer_fetch_unsigned(ScmObj ptr, ScmObj offset, 
                                             ScmObj width);
extern ScmObj yuniffi_pointer_store(ScmObj ptr, ScmObj offset, ScmObj width,
                                    ScmObj data);
extern ScmObj yuniffi_pointer_fromint(ScmObj offset);

extern ScmObj yuniffi_pointer_fetch_p64(ScmObj ptr, ScmObj offset);
extern ScmObj yuniffi_pointer_fetch_p64_bv(ScmObj bv, ScmObj offset);
extern ScmObj yuniffi_pointer_store_p64(ScmObj ptr, ScmObj offset, ScmObj data);
extern ScmObj yuniffi_pointer_store_p64_bv(ScmObj bv, ScmObj offset,
                                           ScmObj data);

/* Epilogue */
SCM_DECL_END

#endif  /* GAUCHE_YUNIFFI_H */

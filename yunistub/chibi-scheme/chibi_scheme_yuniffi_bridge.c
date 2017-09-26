/* Hand-written chibi-scheme stub module for callbridge */

#include <chibi/eval.h>
#include "yuniffi_stub.inc.c"

#define REQUIRE(ctx, self, arg, fn, typ) \
    if(!fn(arg)) return sexp_type_exception(ctx, self, typ, arg)

#define REQUIRE_TAG(ctx, self, arg, fn,typ) \
    if(!(fn(arg) && (sexp_pointer_tag(arg) == typ))) \
        return sexp_type_exception(ctx, self, typ, arg)

static sexp
sexp_yuniffi_nccc_proc_register(sexp ctx, sexp self, sexp_sint_t n,
                                sexp proc){
    sexp_gc_var2(res, resptr);
    REQUIRE(ctx, self, proc, sexp_procedurep, SEXP_PROCEDURE);
    sexp_gc_preserve2(ctx, res, resptr);
    res = sexp_cons(ctx, ctx, proc);
    resptr = sexp_make_cpointer(ctx, SEXP_CPOINTER, (void*)(uintptr_t)res,
                                 SEXP_FALSE, 0);
    sexp_preserve_object(ctx, res);
    sexp_gc_release2(ctx);
    return resptr;
}

static sexp
sexp_yuniffi_nccc_proc_release(sexp ctx, sexp self, sexp_sint_t n,
                               sexp procobjptr){
    sexp procobj;
    REQUIRE_TAG(ctx, self, procobjptr, sexp_pointerp, SEXP_CPOINTER);
    procobj = (sexp)sexp_cpointer_value(procobjptr);
    sexp_release_object(ctx, procobj);
    return SEXP_VOID;
}

static sexp
sexp_yuniffi_nccc_call_bridge(sexp ctx, sexp self, sexp_sint_t n,
                              sexp func,
                              sexp in,  sexp in_offs,  sexp in_len,
                              sexp out, sexp out_offs, sexp out_len){
    sexp res;
    REQUIRE_TAG(ctx, self, func, sexp_pointerp, SEXP_CPOINTER);
    REQUIRE(ctx, self, in_offs, sexp_exact_integerp, SEXP_FIXNUM);
    REQUIRE(ctx, self, in_len, sexp_exact_integerp, SEXP_FIXNUM);
    REQUIRE(ctx, self, out_offs, sexp_exact_integerp, SEXP_FIXNUM);
    REQUIRE(ctx, self, out_len, sexp_exact_integerp, SEXP_FIXNUM);

    yuniffi_nccc_call(ctx, (void*)sexp_cpointer_value(func),
                      in, 
                      sexp_sint_value(in_offs), 
                      sexp_sint_value(in_len),
                      out,
                      sexp_sint_value(out_offs), 
                      sexp_sint_value(out_len));

    return SEXP_VOID;
}

sexp
sexp_init_library(sexp ctx, sexp self, sexp_sint_t n, sexp env,
                  const char* version, const sexp_abi_identifier_t abi){
    sexp_gc_var3(name, tmp, op);
    sexp args;
    /* check ABI */
    if(!(sexp_version_compatible(ctx, version, sexp_version)
         && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER))) {
        return SEXP_ABI_ERROR;
    }
    sexp_gc_preserve3(ctx, name, tmp, op);

    op = sexp_define_foreign(ctx, env, "yuniffi_nccc_call", 7,
                             sexp_yuniffi_nccc_call_bridge);
    if(sexp_opcodep(op)){
        sexp_opcode_return_type(op) = SEXP_VOID;
        /* 1: func   */ sexp_opcode_arg2_type(op) = SEXP_VOID;
        /* 2: in     */
        /* 3: in_off */ sexp_opcode_arg3_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
        /* N: [1:in_len, 2:out, 3:out_off, 4:out_len] */
        sexp_opcode_argn_type(op) = 
            sexp_make_vector(ctx, SEXP_FOUR, sexp_make_fixnum(SEXP_OBJECT));
        args = sexp_opcode_argn_type(op);
        sexp_vector_set(args, SEXP_ZERO,  sexp_make_fixnum(SEXP_FIXNUM));
        sexp_vector_set(args, SEXP_TWO,   sexp_make_fixnum(SEXP_FIXNUM));
        sexp_vector_set(args, SEXP_THREE, sexp_make_fixnum(SEXP_FIXNUM));
    }else{
        /* FIXME: abort() here? */
    }

    op = sexp_define_foreign(ctx, env, "yuniffi_nccc_proc_register", 1,
                             sexp_yuniffi_nccc_proc_register);
    if(sexp_opcodep(op)){
        sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_CPOINTER);
        /* 1: proc   */ sexp_opcode_arg1_type(op) = sexp_make_fixnum(SEXP_PROCEDURE);
    }

    op = sexp_define_foreign(ctx, env, "yuniffi_nccc_proc_release", 1,
                             sexp_yuniffi_nccc_proc_release);
    if(sexp_opcodep(op)){
        sexp_opcode_return_type(op) = SEXP_VOID;
        /* 1: proc   */ sexp_opcode_arg1_type(op) = sexp_make_fixnum(SEXP_CPOINTER);
    }

    sexp_gc_release3(ctx);
    return SEXP_VOID;
}

/* Handwritten C/C++ polygot */

#define MUST_BE_DEFINED
#define MUST_BE_DEFINED2
#define MUST_BE_DEFINED3
#define MUST_BE_DEFINED4
#define MUST_BE_DEFINED5
#define MUST_BE_DEFINED6

#include "testing.h"
#include <stdio.h>
#include "testinterface.h"

#include <yuniffi/stub/_top.h>

/* FIXME: TYPE == TYPE_BLOB ... Perhaps we need a flag to indicate
 * integer type */

#define EXPORT_someint_t(k)\
    YUNIFFI_EXPORTCLASS_TYPE(k, "someint_t", someint_t)

#define EXPORT_someenum_t(k)\
    YUNIFFI_EXPORTCLASS_TYPE(k, "someenum_t", someenum_t)

#define EXPORT_someenum2_e(k)\
    YUNIFFI_EXPORTCLASS_TYPE(k, "someenum2_e", enum someenum2_e)

#define EXPORT_somestruct_s(k)\
    YUNIFFI_EXPORTCLASS_TYPE_BLOB(k, "somestruct_s", struct somestruct_s)

#define EXPORT_someunion_u(k)\
    YUNIFFI_EXPORTCLASS_TYPE_BLOB(k, "someunion_u", union someunion_u)

#define EXPORT_someblob_t(k)\
    YUNIFFI_EXPORTCLASS_TYPE_BLOB(k, "someblob_t", someblob_t)

#define EXPORT_someenum_t_ENUM_VAL_1(k)\
    YUNIFFI_EXPORTCLASS_CONST_SINT(k, "someenum_t/ENUM_VAL_1", ENUM_VAL_1)

#define EXPORT_someenum_t_ENUM_VAL_2(k)\
    YUNIFFI_EXPORTCLASS_CONST_SINT(k, "someenum_t/ENUM_VAL_2", ENUM_VAL_2)

#define EXPORT_someenum2_e_ENUM2_VAL_1(k)\
    YUNIFFI_EXPORTCLASS_CONST_SINT(k, "someenum2_e/ENUM2_VAL_1", ENUM2_VAL_1)

#define EXPORT_someenum2_e_ENUM2_VAL_2(k)\
    YUNIFFI_EXPORTCLASS_CONST_SINT(k, "someenum2_e/ENUM2_VAL_2", ENUM2_VAL_2)

#define EXPORT_FLG_1(k)\
    YUNIFFI_EXPORTCLASS_CONST_UINT(k, "FLG_1", FLG_1)

#define EXPORT_FLG_2(k)\
    YUNIFFI_EXPORTCLASS_CONST_UINT(k, "FLG_2", FLG_2)

#define EXPORT_someblob_t_a(k)\
    YUNIFFI_EXPORTCLASS_AGGREGATE_MEMBER(k, "someblob_t/a", someblob_t, a)
#define EXPORT_someblob_t_b(k)\
    YUNIFFI_EXPORTCLASS_AGGREGATE_MEMBER(k, "someblob_t/b", someblob_t, b)

#define EXPORT_somestruct_s_fnum(k)\
    YUNIFFI_EXPORTCLASS_AGGREGATE_MEMBER(k, "somestruct_s/fnum", struct somestruct_s, fnum)
#define EXPORT_somestruct_s_someinteger(k)\
    YUNIFFI_EXPORTCLASS_AGGREGATE_MEMBER(k, "somestruct_s/someinteger", struct somestruct_s, someinteger)

#define EXPORT_somestruct_s_someintegers(k)\
    YUNIFFI_EXPORTCLASS_AGGREGATE_MEMBER(k, "somestruct_s/someintegers", struct somestruct_s, someintegers)

#define EXPORT_somestruct_s_bufferlength(k)\
    YUNIFFI_EXPORTCLASS_AGGREGATE_MEMBER(k, "somestruct_s/bufferlength", struct somestruct_s, bufferlength)

#define EXPORT_somestruct_s_buffer(k)\
    YUNIFFI_EXPORTCLASS_AGGREGATE_MEMBER(k, "somestruct_s/buffer", struct somestruct_s, buffer)

#define EXPORT_somestruct_s_someconst(k)\
    YUNIFFI_EXPORTCLASS_AGGREGATE_MEMBER(k, "somestruct_s/someconst", struct somestruct_s, someconst)

#define EXPORT_somestruct_s_someenum2(k)\
    YUNIFFI_EXPORTCLASS_AGGREGATE_MEMBER(k, "somestruct_s/someenum2", struct somestruct_s, someenum2)

#define EXPORT_somestruct_s_someintlength(k)\
    YUNIFFI_EXPORTCLASS_AGGREGATE_MEMBER(k, "somestruct_s/someintlength", struct somestruct_s, someintlength)

#define EXPORT_somestruct_s_someintbytelength(k)\
    YUNIFFI_EXPORTCLASS_AGGREGATE_MEMBER(k, "somestruct_s/someintbytelength", struct somestruct_s, someintbytelength)

#define EXPORT_somestruct_s_someints(k)\
    YUNIFFI_EXPORTCLASS_AGGREGATE_MEMBER(k, "somestruct_s/someints", struct somestruct_s, someints)

#ifdef CONST_1
#define EXPORT_CONST_1(k)\
    YUNIFFI_EXPORTCLASS_CONST_SINT(k, "CONST_1", CONST_1)
#endif

#define EXPORT_MINUS_CONST_1(k)\
    YUNIFFI_EXPORTCLASS_CONST_SINT(k, "MINUS_CONST_1", MINUS_CONST_1)

#define EXPORT_REAL_1(k)\
    YUNIFFI_EXPORTCLASS_CONST_REAL(k, "REAL_1", REAL_1)

#define EXPORT_CONST_2(k)\
    YUNIFFI_EXPORTCLASS_CONST_SINT(k, "CONST_2", CONST_2)

#define EXPORT_REAL_2(k)\
    YUNIFFI_EXPORTCLASS_CONST_REAL(k, "REAL_2", REAL_2)

#define EXPORT_size_of_someblob_t(k)\
    YUNIFFI_EXPORTCLASS_CONST_SINT(k, "size_of_someblob_t", size_of_someblob_t)

#define EXPORT_size_of_someunion_u(k)\
    YUNIFFI_EXPORTCLASS_CONST_SINT(k, "size_of_someunion_u", size_of_someunion_u)
#define EXPORT_size_of_somestruct_s(k)\
    YUNIFFI_EXPORTCLASS_CONST_SINT(k, "size_of_somestruct_s", size_of_somestruct_s)
#define EXPORT_size_of_someopaque_t(k)\
    YUNIFFI_EXPORTCLASS_CONST_SINT(k, "size_of_someopaque_t", size_of_someopaque_t)


#include <yuniffi/stub/_begin_constants.h>


YUNIFFI_C_BEGIN

YUNIFFI_EXPORTFUNC_BEGIN(export_constants)
    YUNIFFI_EXPORTFUNC_ENTRY(1,EXPORT_someint_t)
    YUNIFFI_EXPORTFUNC_ENTRY(2,EXPORT_someenum_t)
    YUNIFFI_EXPORTFUNC_ENTRY(3,EXPORT_someenum2_e)
    YUNIFFI_EXPORTFUNC_ENTRY(4,EXPORT_somestruct_s)
    YUNIFFI_EXPORTFUNC_ENTRY(5,EXPORT_someunion_u)
    YUNIFFI_EXPORTFUNC_ENTRY(6,EXPORT_someblob_t)
    YUNIFFI_EXPORTFUNC_ENTRY(7,EXPORT_someenum_t_ENUM_VAL_1)
    YUNIFFI_EXPORTFUNC_ENTRY(8,EXPORT_someenum_t_ENUM_VAL_2)
    YUNIFFI_EXPORTFUNC_ENTRY(9,EXPORT_someenum2_e_ENUM2_VAL_1)
    YUNIFFI_EXPORTFUNC_ENTRY(10,EXPORT_someenum2_e_ENUM2_VAL_2)
    YUNIFFI_EXPORTFUNC_ENTRY(11,EXPORT_FLG_1)
    YUNIFFI_EXPORTFUNC_ENTRY(12,EXPORT_FLG_2)
    YUNIFFI_EXPORTFUNC_ENTRY(13,EXPORT_someblob_t_a)
    YUNIFFI_EXPORTFUNC_ENTRY(14,EXPORT_someblob_t_b)
    YUNIFFI_EXPORTFUNC_ENTRY(15,EXPORT_somestruct_s_fnum)
    YUNIFFI_EXPORTFUNC_ENTRY(16,EXPORT_somestruct_s_someinteger)
    YUNIFFI_EXPORTFUNC_ENTRY(17,EXPORT_somestruct_s_someintegers)
    YUNIFFI_EXPORTFUNC_ENTRY(18,EXPORT_somestruct_s_bufferlength)
    YUNIFFI_EXPORTFUNC_ENTRY(19,EXPORT_somestruct_s_buffer)
    YUNIFFI_EXPORTFUNC_ENTRY(20,EXPORT_somestruct_s_someconst)
    YUNIFFI_EXPORTFUNC_ENTRY(21,EXPORT_somestruct_s_someenum2)
    YUNIFFI_EXPORTFUNC_ENTRY(22,EXPORT_somestruct_s_someintlength)
    YUNIFFI_EXPORTFUNC_ENTRY(23,EXPORT_somestruct_s_someintbytelength)
    YUNIFFI_EXPORTFUNC_ENTRY(24,EXPORT_somestruct_s_someints)
#ifdef CONST_1
    YUNIFFI_EXPORTFUNC_ENTRY(25,EXPORT_CONST_1)
#endif
    YUNIFFI_EXPORTFUNC_ENTRY(26,EXPORT_MINUS_CONST_1)
    YUNIFFI_EXPORTFUNC_ENTRY(27,EXPORT_REAL_1)
    YUNIFFI_EXPORTFUNC_ENTRY(28,EXPORT_CONST_2)
    YUNIFFI_EXPORTFUNC_ENTRY(29,EXPORT_REAL_2)
    YUNIFFI_EXPORTFUNC_ENTRY(30,EXPORT_size_of_someblob_t)
    YUNIFFI_EXPORTFUNC_ENTRY(31,EXPORT_size_of_someunion_u)
    YUNIFFI_EXPORTFUNC_ENTRY(32,EXPORT_size_of_somestruct_s)
    YUNIFFI_EXPORTFUNC_ENTRY(33,EXPORT_size_of_someopaque_t)
YUNIFFI_EXPORTFUNC_END(export_constants)

YUNIFFI_C_END

#include <yuniffi/stub/_begin_bridge.h>

static
YUNIFFI_FUNC_BEGIN(test_constcharptr_forward0,in,in_size,out,out_size)
    /* out[0] : voidptr [return] */
    const char* out0;

    /* call */
    out0 = test_constcharptr();

    /* output */
    YUNIWORD_SET_PTR(out,0,out0);
YUNIFFI_FUNC_END(test_constcharptr_forward0)

static
YUNIFFI_FUNC_BEGIN(test_constcharptr_check_forward0,in,in_size,out,out_size)
    /* in[0] : int [in] */
    int const in0 = (int)YUNIWORD_REF_SINT(in,0);
    /* out[0] : int [return] */
    int out0;

    /* call */
    out0 = test_constcharptr_check(in0);

    /* output */
    YUNIWORD_SET_SINT(out,0,out0);
YUNIFFI_FUNC_END(test_constcharptr_check_forward0)

static
YUNIFFI_FUNC_BEGIN(test_constvoidptr_forward0,in,in_size,out,out_size)
    /* out[0] : voidptr [return] */
    const void* out0;

    /* call */
    out0 = test_constvoidptr();

    /* output */
    YUNIWORD_SET_PTR(out,0,out0);
YUNIFFI_FUNC_END(test_constvoidptr_forward0)

static
YUNIFFI_FUNC_BEGIN(test_constvoidptr_check_forward0,in,in_size,out,out_size)
    /* in[0] : voidptr [in] */
    void* const in0 = (void*)YUNIWORD_REF_PTR(in,0);

    /* out[0] : int [return] */
    int out0;

    /* call */
    out0 = test_constvoidptr_check(in0);

    /* output */
    YUNIWORD_SET_SINT(out,0,out0);
YUNIFFI_FUNC_END(test_constvoidptr_check_forward0)

static
YUNIFFI_FUNC_BEGIN(test_intvec_forward0,in,in_size,out,out_size)
    /* in[0] : intvec [in] */
    int* const in0 = (int*)YUNIWORD_REF_PTR(in,0);
    /* in[1] : int [count] */
    int const in1 = (int)YUNIWORD_REF_SINT(in,1);
    /* in[2] : int [size] */
    int const in2 = (int)YUNIWORD_REF_SINT(in,2);
    /* out[0] : int [return] */
    int out0;

    /* call */
    out0 = test_intvec(in0,in1,in2);

    /* output */
    YUNIWORD_SET_SINT(out,0,out0);
YUNIFFI_FUNC_END(test_intvec_forward0)

static
YUNIFFI_FUNC_BEGIN(test_someblob_t_1_forward0,in,in_size,out,out_size)
    /* in[0] : someblobptr [in] */
    someblob_t* const in0 = (someblob_t*)YUNIWORD_REF_PTR(in,0);
    /* out[0] : int [return] */
    int out0;

    /* call */
    out0 = test_someblob_t_1(in0);

    /* output */
    YUNIWORD_SET_SINT(out,0,out0);
YUNIFFI_FUNC_END(test_someblob_t_1_forward0)

static
YUNIFFI_FUNC_BEGIN(test_someblob_t_2_forward0,in,in_size,out,out_size)
    /* in[0] : someblobvec [in] */
    someblob_t* const in0 = (someblob_t*)YUNIWORD_REF_PTR(in,0);
    /* out[0] : int [return] */
    int out0;

    /* call */
    out0 = test_someblob_t_2(in0);

    /* output */
    YUNIWORD_SET_SINT(out,0,out0);
YUNIFFI_FUNC_END(test_someblob_t_2_forward0)

static
YUNIFFI_FUNC_BEGIN(test_someunion_u_1_forward0,in,in_size,out,out_size)
    /* in[0] : someunionptr [in] */
    union someunion_u* const in0 = (union someunion_u*)YUNIWORD_REF_PTR(in,0);
    /* out[0] : int [return] */
    int out0;

    /* call */
    out0 = test_someunion_u_1(in0);

    /* output */
    YUNIWORD_SET_SINT(out,0,out0);
YUNIFFI_FUNC_END(test_someunion_u_1_forward0)

static
YUNIFFI_FUNC_BEGIN(test_someunion_u_1a_forward0,in,in_size,out,out_size)
    /* in[0] : someunionptr [in] */
    union someunion_u* const in0 = (union someunion_u*)YUNIWORD_REF_PTR(in,0);
    /* out[0] : int [return] */
    int out0;

    /* call */
    out0 = test_someunion_u_1a(in0);

    /* output */
    YUNIWORD_SET_SINT(out,0,out0);
YUNIFFI_FUNC_END(test_someunion_u_1a_forward0)

static
YUNIFFI_FUNC_BEGIN(test_someunion_u_2_forward0,in,in_size,out,out_size)
    /* in[0] : someunionptr [out] */
    union someunion_u* const in0 = (union someunion_u*)YUNIWORD_REF_PTR(in,0);
    /* out[0] : int [return] */
    int out0;

    /* call */
    out0 = test_someunion_u_2(in0);

    /* output */
    YUNIWORD_SET_SINT(out,0,out0);
YUNIFFI_FUNC_END(test_someunion_u_2_forward0)

static
YUNIFFI_FUNC_BEGIN(test_somestruct_1_forward0,in,in_size,out,out_size)
    /* in[0] : somestructptr [in] */
    struct somestruct_s* const in0 = (struct somestruct_s*)YUNIWORD_REF_PTR(in,0);
    /* out[0] : int [return] */
    int out0;

    /* call */
    out0 = test_somestruct_1(in0);

    /* output */
    YUNIWORD_SET_SINT(out,0,out0);
YUNIFFI_FUNC_END(test_somestruct_1_forward0)

static
YUNIFFI_FUNC_BEGIN(test_somestruct_2_forward0,in,in_size,out,out_size)
    /* in[0] : somestructptr [out] */
    struct somestruct_s* const in0 = (struct somestruct_s*)YUNIWORD_REF_PTR(in,0);
    /* out[0] : int [return] */
    int out0;

    /* call */
    out0 = test_somestruct_2(in0);

    /* output */
    YUNIWORD_SET_SINT(out,0,out0);
YUNIFFI_FUNC_END(test_somestruct_2_forward0)

static
YUNIFFI_FUNC_BEGIN(test_get_somestruct_1f_test_forward0,in,in_size,out,out_size)
    /* out[0] : voidptr [return] */
    void* out0;

    out0 = test_get_somestruct_1f_test();

    /* output */
    YUNIWORD_SET_PTR(out,0,out0);
YUNIFFI_FUNC_END(test_get_somestruct_1f_test_forward0)

static
YUNIFFI_FUNC_BEGIN(test_somestruct_1f_forward1,in,in_size,out,out_size)
    /* in[0] : test_somestruct_1f function */
    typedef int (*test_somestruct_1f_forward1_func)(struct somestruct_s*);
    test_somestruct_1f_forward1_func const in0 = (test_somestruct_1f_forward1_func)YUNIWORD_REF_PTR(in,0);
    /* in[1] : somestructptr [in] */
    struct somestruct_s* const in1 = (struct somestruct_s*)YUNIWORD_REF_PTR(in,1);
    /* out[0] : int [return] */
    int out0;

    /* call */
    out0 = in0(in1);

    /* output */
    YUNIWORD_SET_SINT(out,0,out0);
YUNIFFI_FUNC_END(test_somestruct_1f_forward1)

static
YUNIFFI_FUNC_BEGIN(test_outint_forward0,in,in_size,out,out_size)
    /* out[0] : int [return] */
    int out0;
    /* out[1] : int [thevalue] */
    int out1;

    /* call */
    out0 = test_outint(&out1);

    /* output */
    YUNIWORD_SET_SINT(out,0,out0);
    YUNIWORD_SET_SINT(out,1,out1);
YUNIFFI_FUNC_END(test_outint_forward0)

#define EXPORT_test_constcharptr_forward0(k) YUNIFFI_EXPORTPROC(k,test_constcharptr_forward0)
#define EXPORT_test_constcharptr_check_forward0(k) YUNIFFI_EXPORTPROC(k,test_constcharptr_check_forward0)
#define EXPORT_test_constvoidptr_forward0(k) YUNIFFI_EXPORTPROC(k,test_constvoidptr_forward0)
#define EXPORT_test_constvoidptr_check_forward0(k) YUNIFFI_EXPORTPROC(k,test_constvoidptr_check_forward0)
#define EXPORT_test_intvec_forward0(k) YUNIFFI_EXPORTPROC(k,test_intvec_forward0)
#define EXPORT_test_someblob_t_1_forward0(k) YUNIFFI_EXPORTPROC(k,test_someblob_t_1_forward0)
#define EXPORT_test_someblob_t_2_forward0(k) YUNIFFI_EXPORTPROC(k,test_someblob_t_2_forward0)
#define EXPORT_test_someunion_u_1_forward0(k) YUNIFFI_EXPORTPROC(k,test_someunion_u_1_forward0)
#define EXPORT_test_someunion_u_1a_forward0(k) YUNIFFI_EXPORTPROC(k,test_someunion_u_1a_forward0)
#define EXPORT_test_someunion_u_2_forward0(k) YUNIFFI_EXPORTPROC(k,test_someunion_u_2_forward0)
#define EXPORT_test_somestruct_1_forward0(k) YUNIFFI_EXPORTPROC(k,test_somestruct_1_forward0)
#define EXPORT_test_somestruct_2_forward0(k) YUNIFFI_EXPORTPROC(k,test_somestruct_2_forward0)
#define EXPORT_test_get_somestruct_1f_test_forward0(k) YUNIFFI_EXPORTPROC(k,test_get_somestruct_1f_test_forward0)
#define EXPORT_test_somestruct_1f_forward1(k) YUNIFFI_EXPORTPROC(k,test_somestruct_1f_forward1)
#define EXPORT_test_outint_forward0(k) YUNIFFI_EXPORTPROC(k,test_outint_forward0)

YUNIFFI_C_BEGIN

YUNIFFI_EXPORTFUNC_BEGIN(export_bridgestubs)
    YUNIFFI_EXPORTFUNC_ENTRY(1,EXPORT_test_constcharptr_forward0)
    YUNIFFI_EXPORTFUNC_ENTRY(2,EXPORT_test_constcharptr_check_forward0)
    YUNIFFI_EXPORTFUNC_ENTRY(3,EXPORT_test_constvoidptr_forward0)
    YUNIFFI_EXPORTFUNC_ENTRY(4,EXPORT_test_constvoidptr_check_forward0)
    YUNIFFI_EXPORTFUNC_ENTRY(5,EXPORT_test_intvec_forward0)
    YUNIFFI_EXPORTFUNC_ENTRY(6,EXPORT_test_someblob_t_1_forward0)
    YUNIFFI_EXPORTFUNC_ENTRY(7,EXPORT_test_someblob_t_2_forward0)
    YUNIFFI_EXPORTFUNC_ENTRY(8,EXPORT_test_someunion_u_1_forward0)
    YUNIFFI_EXPORTFUNC_ENTRY(9,EXPORT_test_someunion_u_1a_forward0)
    YUNIFFI_EXPORTFUNC_ENTRY(10,EXPORT_test_someunion_u_2_forward0)
    YUNIFFI_EXPORTFUNC_ENTRY(11,EXPORT_test_somestruct_1_forward0)
    YUNIFFI_EXPORTFUNC_ENTRY(12,EXPORT_test_somestruct_2_forward0)
    YUNIFFI_EXPORTFUNC_ENTRY(13,EXPORT_test_get_somestruct_1f_test_forward0)
    YUNIFFI_EXPORTFUNC_ENTRY(14,EXPORT_test_somestruct_1f_forward1)
    YUNIFFI_EXPORTFUNC_ENTRY(15,EXPORT_test_outint_forward0)
YUNIFFI_EXPORTFUNC_END(export_bridgestubs)

YUNIFFI_C_END

#include <yuniffi/stub/_end.h>

#ifndef YUNIFFI_STUB_TEMPLATES_H
#define YUNIFFI_STUB_TEMPLATES_H

/* Code templates for yuniffi stub librarires */

#ifdef __cplusplus
#define YUNIFFI_C_BEGIN extern "C" {
#define YUNIFFI_C_END }
#else
#define YUNIFFI_C_BEGIN
#define YUNIFFI_C_END
#endif

/* FIXME: Support visibility, embedded stubs */

#if defined(_WIN32)
#define YUNIFFI_STUB_EXPORT __declspec(dllexport)
#else
#define YUNIFFI_STUB_EXPORT
#endif

/* Object metadata template */

#include <stddef.h> /* For offsetof */

#define YUNIFFI_OFFSETOF offsetof
#define YUNIFFI_AGGREGATE_SIZEOF(type,name) \
    sizeof(((type *)0) -> name)

/* Function templates */

/*  Core function */

#define YUNIFFI_FUNC_BEGIN(name,in,in_size,out,out_size) \
    void name(yuniword_t* in, yunioffs_t in_size, \
              yuniword_t* out, yunioffs_t out_size) {

#define YUNIFFI_FUNC_END(name) \
    }

/*  
 *  Forward stub template ::
 */


/*  
 *  Export function template ::
 *
 *   Export functions are 4in-8out function exports a symbol metadata
 *   to client.
 *
 *       in[0] func      : function
 *             0 : Import
 *             1 : Configure
 *
 *       in[1] selector  : object offset
 *       in[2] pointer   : input
 *       in[3] reserved0 :
 *
 *        Func 0 : Import
 *         out[0] flags : Symbol flags <symflags.h> 
 *         out[1] name_pointer : Symbol name pointer
 *         out[2] name_size : Symbol name size
 *         out[3] value : Symbol value
 *         out[4] size : Symbol size YUNIFFI_SYMBOL__HAS_SIZE
 *         out[5] offset : Symbol offset YUNIFFI_SYMBOL__HAS_OFFSET 
 *         out[6] reserved0
 *         out[7] reserved1 */


typedef void(*yuni_nccc_proc_t)(uint64_t* in, int in_len,
                                uint64_t* out, int out_len);
/* Tentative */
#define YUNIFFI_STUB_GENERIC_TRAMPOLINE(name) \
    static void name ## _generic_trampoline(uintptr_t proc,\
                                     uint64_t* in, int in_len,\
                                     uint64_t* out, int out_len){\
        const yuni_nccc_proc_t func = (yuni_nccc_proc_t)proc;\
        func(in, in_len, out, out_len);\
    }

#define YUNIFFI_STUB_GLOBAL(name) \
    YUNIFFI_STUB_GENERIC_TRAMPOLINE(name)\
    uintptr_t name ## _callback_ptr = \
    (uintptr_t)name ## _generic_trampoline;

#define YUNIFFI_EXPORTFUNC_BEGIN(name) \
    YUNIFFI_STUB_GLOBAL(name) \
    YUNIFFI_STUB_EXPORT \
    YUNIFFI_FUNC_BEGIN(name,in,in_size,out,out_size) \
    const int func = (int)YUNIWORD_REF_UINT(in,0);\
    const int selector = (int)YUNIWORD_REF_UINT(in,1);\
    const uintptr_t pointer = (uintptr_t)YUNIWORD_REF_UINT(in,2);\
    if((func == 1) && (selector == 0)){\
        name ## _callback_ptr = pointer;\
        return;\
    }\
    if(func != 0){\
        YUNIWORD_SET_UINT(out,0,0);\
        return;\
    }\
    switch(selector){\
        default:\
        YUNIWORD_SET_UINT(out,0,0);\
        break;

#define YUNIFFI_EXPORTFUNC_ENTRY(sel,m)\
        case sel:\
        YUNIWORD_SET_UINT(out,0,YUNIFFI_EXPORT_FLAGS(m));\
        YUNIWORD_SET_PTR(out,1,YUNIFFI_EXPORT_NAME_POINTER(m));\
        YUNIWORD_SET_UINT(out,2,YUNIFFI_EXPORT_NAME_SIZE(m));\
        YUNIFFI_EXPORT_VALUE_SETTER(m)(out,3,YUNIFFI_EXPORT_VALUE(m));\
        YUNIWORD_SET_UINT(out,4,YUNIFFI_EXPORT_SIZE(m));\
        YUNIWORD_SET_UINT(out,5,YUNIFFI_EXPORT_OFFSET(m));\
        YUNIWORD_SET_UINT(out,6,0);\
        YUNIWORD_SET_UINT(out,7,0);\
        break;

#define YUNIFFI_EXPORTFUNC_ENTRY_TERM(sel)\
        case sel:\
        YUNIWORD_SET_UINT(out,0,YUNIFFI_SYMBOL_TERMINATE);\
        YUNIWORD_SET_PTR(out,1,0);\
        YUNIWORD_SET_PTR(out,2,0);\
        YUNIWORD_SET_PTR(out,3,0);\
        YUNIWORD_SET_PTR(out,4,0);\
        YUNIWORD_SET_PTR(out,5,0);\
        YUNIWORD_SET_PTR(out,6,0);\
        YUNIWORD_SET_PTR(out,7,0);\
        break;

#define YUNIFFI_EXPORTFUNC_END(name) \
    }\
    YUNIFFI_FUNC_END(name)

/*  Export datum templates ::
 *
 *   A raw export will be a CPS macro like this;
 *
 *    #define RAWEXPORT(k) k(r0,r1,r2,r3,r4,r5,r6,r7)
 *
 *  Current mapping is:
 *
 *    r0 : flags
 *    r1 : name (C string)
 *    r2 : value_setter such as YUNIWORD_SET_SINT
 *    r3 : value (argument to value_setter macro)
 *    r4 : size
 *    r5 : offset
 *
 *  To encapsulate raw mapping, yuniFFI offer several templates.
 *  Don't use RAWEXPORT directly, use EXPORTCLASS instead.
 *
 *    int x_value = 100;
 *    #define EXPORT_X(k) \
 *        YUNIFFI_EXPORTCLASS_CONST_SINT(k, "x", x_value)
 *
 */

#define YUNIFFI_EXPORTCLASS_CONST_SINT(k,str,value)\
    k(YUNIFFI_SYMBOL_CONSTANT, str, YUNIWORD_SET_SINT, value, 0, 0, \
      INVALID, INVALID)

#define YUNIFFI_EXPORTCLASS_CONST_UINT(k,str,value)\
    k(YUNIFFI_SYMBOL_CONSTANT, str, YUNIWORD_SET_UINT, value, 0, 0, \
      INVALID, INVALID)

#define YUNIFFI_EXPORTCLASS_CONST_PTR(k,str,value)\
    k(YUNIFFI_SYMBOL_CONSTANT, str, YUNIWORD_SET_PTR, value, 0, 0, \
      INVALID, INVALID)

#define YUNIFFI_EXPORTCLASS_CONST_REAL(k,str,value)\
    k(YUNIFFI_SYMBOL_CONSTANT, str, YUNIWORD_SET_DOUBLE, value, 0, 0, \
      INVALID, INVALID)

#define YUNIFFI_EXPORTCLASS_CONST_BLOB(k,str,ptr,size)\
    k(YUNIFFI_SYMBOL_CONSTANT_BLOB, str, YUNIWORD_SET_PTR, ptr, size, 0, \
      INVALID, INVALID)

#define YUNIFFI_EXPORTCLASS_TYPE(k,str,x)\
    k(YUNIFFI_SYMBOL_TYPE, str, YUNIWORD_SET_SINT /* UNUSED */, 0, \
      sizeof(x), 0, INVALID, INVALID)

#define YUNIFFI_EXPORTCLASS_TYPE_BLOB(k,str,x)\
    k(YUNIFFI_SYMBOL_TYPE, str, YUNIWORD_SET_SINT /* UNUSED */, 0, \
      sizeof(x), 0, INVALID, INVALID)

#define YUNIFFI_EXPORTCLASS_AGGREGATE_MEMBER(k,str,base,name)\
    k(YUNIFFI_SYMBOL_AGGREGATE_MEMBER, str, YUNIWORD_SET_SINT /* UNUSED */, 0,\
      YUNIFFI_AGGREGATE_SIZEOF(base,name), YUNIFFI_OFFSETOF(base,name), \
      INVALID, INVALID)

#define YUNIFFI_EXPORTPROC(k,sym) YUNIFFI_EXPORTCLASS_CONST_PTR(k,#sym,sym)
#define YUNIFFI_EXPORTPROC_F0(k,sym) YUNIFFI_EXPORTCLASS_CONST_PTR(k,#sym,sym)
#define YUNIFFI_EXPORTPROC_F1(k,sym) YUNIFFI_EXPORTCLASS_CONST_PTR(k,#sym,sym)
#define YUNIFFI_EXPORTPROC_B1(k,sym) YUNIFFI_EXPORTCLASS_CONST_PTR(k,#sym,sym)
#define YUNIFFI_EXPORTPROC_B2(k,sym) YUNIFFI_EXPORTCLASS_CONST_PTR(k,#sym,sym)

#define YUNIFFI_RAWEXPORT_R0(r0,r1,r2,r3,r4,r5,r6,r7) r0
#define YUNIFFI_RAWEXPORT_R1(r0,r1,r2,r3,r4,r5,r6,r7) r1
#define YUNIFFI_RAWEXPORT_R2(r0,r1,r2,r3,r4,r5,r6,r7) r2
#define YUNIFFI_RAWEXPORT_R3(r0,r1,r2,r3,r4,r5,r6,r7) r3
#define YUNIFFI_RAWEXPORT_R4(r0,r1,r2,r3,r4,r5,r6,r7) r4
#define YUNIFFI_RAWEXPORT_R5(r0,r1,r2,r3,r4,r5,r6,r7) r5
#define YUNIFFI_RAWEXPORT_R6(r0,r1,r2,r3,r4,r5,r6,r7) r6
#define YUNIFFI_RAWEXPORT_R7(r0,r1,r2,r3,r4,r5,r6,r7) r7

#define YUNIFFI_EXPORT_FLAGS(m)        m(YUNIFFI_RAWEXPORT_R0)
#define YUNIFFI_EXPORT_NAME_POINTER(m) m(YUNIFFI_RAWEXPORT_R1)
#define YUNIFFI_EXPORT_NAME_SIZE(m)    sizeof(YUNIFFI_EXPORT_NAME_POINTER(m))
#define YUNIFFI_EXPORT_VALUE_SETTER(m) m(YUNIFFI_RAWEXPORT_R2)
#define YUNIFFI_EXPORT_VALUE(m)        m(YUNIFFI_RAWEXPORT_R3)
#define YUNIFFI_EXPORT_SIZE(m)         m(YUNIFFI_RAWEXPORT_R4)
#define YUNIFFI_EXPORT_OFFSET(m)       m(YUNIFFI_RAWEXPORT_R5)



#endif

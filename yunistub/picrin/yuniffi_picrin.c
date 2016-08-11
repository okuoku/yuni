#include <stdio.h>
#include <stdint.h>
#include "../common/bootstrap.inc.c"

#include "picrin.h"
#include "picrin/extra.h"

static void
yunipointer_mark(pic_state* pic, void* data, 
                 void (*mark)(pic_state*, pic_value)){
    /* Do nothing */
    (void)pic;
    (void)data;
    (void)mark;
}

static void
yunipointer_dtor(pic_state* pic, void* data){
    /* Do nothing */
    (void)pic;
    (void)data;
}

static const pic_data_type yunipointer_type = {
    "yunipointer",
    yunipointer_dtor,
    yunipointer_mark
};

static pic_value
ptr_value(pic_state* pic, uintptr_t val){
    return pic_data_value(pic, (void*)val, &yunipointer_type);
}


static pic_value
nccc_call(pic_state* pic){
    yuniffi_nccc_func_t callee; /* u : yunipointer_type */
    char* in0;      /* b */
    int in_len;
    int in_offset;  /* i */
    int in_count;   /* i */
    char* out0;     /* b */
    int out_len;
    int out_offset; /* i */
    int out_count;  /* i */

    void* in;
    void* out;
    

    (void)pic_get_args(pic, "ubiibii",
                       &callee, &yunipointer_type,
                       &in0, &in_len,
                       &in_offset, &in_count,
                       &out0, &out_len,
                       &out_offset, &out_count);

    in = &in0[in_offset];
    out = &out0[out_offset];

    callee(in, in_count, out, out_len);

    return pic_undef_value(pic);
}

static pic_value
nccc_bootstrap(pic_state* pic){
    return ptr_value(pic, (uintptr_t)yuniffi_bootstrap0);
}


static pic_value
pointer_fetchpointer_unsigned(pic_state* pic){
    void* ptr;  /* u */
    int offset; /* i */
    int width;  /* i */

    void* p;
    uintptr_t res;

    (void)pic_get_args(pic, "uii",
                       &ptr, &yunipointer_type,
                       &offset, &width);

    p = ptr + offset;

    switch(width){
        case 0:
            res = *(uintptr_t *)p;
            break;
        case 1:
            res = *(uint8_t *)p;
            break;
        case 2:
            res = *(uint16_t *)p;
            break;
        case 4:
            res = *(uint32_t *)p;
            break;
        case 8:
            res = *(uint64_t *)p;
            break;
        default:
            pic_panic(pic, "yunipointer fetchpointer_unsigned: unknown size");
    }

    return ptr_value(pic, res);
}

static pic_value
pointer_fetchpointer_signed(pic_state* pic){
    void* ptr;  /* u */
    int offset; /* i */
    int width;  /* i */

    void* p;
    intptr_t res;

    (void)pic_get_args(pic, "uii",
                       &ptr, &yunipointer_type,
                       &offset, &width);

    p = ptr + offset;

    switch(width){
        case 0:
            res = *(intptr_t *)p;
            break;
        case 1:
            res = *(int8_t *)p;
            break;
        case 2:
            res = *(int16_t *)p;
            break;
        case 4:
            res = *(int32_t *)p;
            break;
        case 8:
            res = *(int64_t *)p;
            break;
        default:
            pic_panic(pic, "yunipointer fetchpointer_signed: unknown size");
    }

    return ptr_value(pic, res);
}

static pic_value
pointer_fetch_unsigned(pic_state* pic){
    void* ptr;  /* u */
    int offset; /* i */
    int width;  /* i */

    void* p;
    uintptr_t res;

    (void)pic_get_args(pic, "uii",
                       &ptr, &yunipointer_type,
                       &offset, &width);

    p = ptr + offset;

    switch(width){
        case 1:
            res = *(uint8_t *)p;
            break;
        case 2:
            res = *(uint16_t *)p;
            break;
        default:
            pic_panic(pic, "yunipointer fetch_unsigned: unknown size");
    }

    return pic_int_value(pic, res);
}

static pic_value
pointer_fetch_signed(pic_state* pic){
    void* ptr;  /* u */
    int offset; /* i */
    int width;  /* i */

    void* p;
    intptr_t res;

    (void)pic_get_args(pic, "uii",
                       &ptr, &yunipointer_type,
                       &offset, &width);

    p = ptr + offset;

    switch(width){
        case 1:
            res = *(int8_t *)p;
            break;
        case 2:
            res = *(int16_t *)p;
            break;
        default:
            pic_panic(pic, "yunipointer fetch_signed: unknown size");
    }

    return pic_int_value(pic, res);
}

static pic_value
pointer_store_unsigned(pic_state* pic){
    void* ptr;  /* u */
    int offset; /* i */
    int width;  /* i */
    int value;  /* i */

    void* p;

    (void)pic_get_args(pic, "uiii",
                       &ptr, &yunipointer_type,
                       &offset, &width, &value);

    p = ptr + offset;

    switch(width){
        case 1:
            *(uint8_t *)p = value;
            break;
        case 2:
            *(uint16_t *)p = value;
            break;
        case 4:
            *(uint32_t *)p = value;
            break;
        case 8:
            *(uint64_t *)p = value;
            break;
        default:
            pic_panic(pic, "yunipointer store_unsigned: unknown size");
            
    }

    return pic_undef_value(pic);
}

static pic_value
pointer_store_signed(pic_state* pic){
    void* ptr;  /* u */
    int offset; /* i */
    int width;  /* i */
    int value;  /* i */

    void* p;

    (void)pic_get_args(pic, "uiii",
                       &ptr, &yunipointer_type,
                       &offset, &width, &value);

    p = ptr + offset;

    switch(width){
        case 1:
            *(int8_t *)p = value;
            break;
        case 2:
            *(int16_t *)p = value;
            break;
        case 4:
            *(int32_t *)p = value;
            break;
        case 8:
            *(int64_t *)p = value;
            break;
        default:
            pic_panic(pic, "yunipointer store_signed: unknown size");
            
    }

    return pic_undef_value(pic);
}

static pic_value
pointer_storepointer_unsigned(pic_state* pic){
    void* ptr;  /* u */
    int offset; /* i */
    int width;  /* i */
    void* value;  /* u */

    void* p;

    (void)pic_get_args(pic, "uiiu",
                       &ptr, &yunipointer_type,
                       &offset, &width, 
                       &value, &yunipointer_type);

    p = ptr + offset;

    switch(width){
        case 0:
            *(uintptr_t *)p = (uintptr_t)value;
            break;
        case 1:
            *(uint8_t *)p = (uintptr_t)value;
            break;
        case 2:
            *(uint16_t *)p = (uintptr_t)value;
            break;
        case 4:
            *(uint32_t *)p = (uintptr_t)value;
            break;
        case 8:
            *(uint64_t *)p = (uintptr_t)value;
            break;
        default:
            pic_panic(pic, "yunipointer storepointer_unsigned: unknown size");
            
    }

    return pic_undef_value(pic);
}

static pic_value
pointer_storepointer_signed(pic_state* pic){
    void* ptr;  /* u */
    int offset; /* i */
    int width;  /* i */
    void* value;  /* u */

    void* p;

    (void)pic_get_args(pic, "uiiu",
                       &ptr, &yunipointer_type,
                       &offset, &width, 
                       &value, &yunipointer_type);

    p = ptr + offset;

    switch(width){
        case 0:
            *(intptr_t *)p = (intptr_t)value;
            break;
        case 1:
            *(int8_t *)p = (intptr_t)value;
            break;
        case 2:
            *(int16_t *)p = (intptr_t)value;
            break;
        case 4:
            *(int32_t *)p = (intptr_t)value;
            break;
        case 8:
            *(int64_t *)p = (intptr_t)value;
            break;
        default:
            pic_panic(pic, "yunipointer storepointer_signed: unknown size");
            
    }

    return pic_undef_value(pic);
}

static pic_value
pointer_p(pic_state* pic){
    pic_value obj; /* o */

    (void)pic_get_args(pic, "o", &obj);

    return pic_bool_value(pic, pic_data_p(pic, obj, &yunipointer_type));
}

static pic_value
pointer_frombytevector(pic_state* pic){
    void* ptr;  /* b */
    int len;
    int offset; /* i */
    void* p;

    (void)pic_get_args(pic, "bi", &ptr, &len, &offset);

    p = ptr + offset;

    return ptr_value(pic, (uintptr_t)p);
}

void
pic_init_yuniffi(pic_state* pic){
    pic_deflibrary(pic, "yuniffi-picrin");
    pic_defun(pic, "%%yunipointer?", pointer_p);
    pic_defun(pic, "%%yunipointer_fetch_signed", pointer_fetch_signed);
    pic_defun(pic, "%%yunipointer_fetch_unsigned", pointer_fetch_unsigned);
    pic_defun(pic, "%%yunipointer_store_signed", pointer_store_signed);
    pic_defun(pic, "%%yunipointer_store_unsigned", pointer_store_unsigned);
    pic_defun(pic, "%%yunipointer_fetchpointer_signed", pointer_fetchpointer_signed);
    pic_defun(pic, "%%yunipointer_fetchpointer_unsigned", pointer_fetchpointer_unsigned);
    pic_defun(pic, "%%yunipointer_storepointer_signed", pointer_storepointer_signed);
    pic_defun(pic, "%%yunipointer_storepointer_unsigned", pointer_storepointer_unsigned);
    pic_defun(pic, "%%yunipointer_frombytevector", pointer_frombytevector);
    pic_defun(pic, "%%yuniffi_nccc_call", nccc_call);
    pic_defun(pic, "%%yuniffi_nccc_bootstrap", nccc_bootstrap);
}

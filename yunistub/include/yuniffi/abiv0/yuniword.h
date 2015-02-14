#ifndef YUNIFFI_ABIV0_YUNIWORD_H
#define YUNIFFI_ABIV0_YUNIWORD_H

/* FIXME: Support legacy compilers... */

#include <stdint.h>

typedef uint64_t yuniword_t;
typedef int yunioffs_t;

#define YUNIWORD_REF_SINT(words,offs) \
    (int64_t)(words[offs])

#define YUNIWORD_REF_UINT(words,offs) \
    (uint64_t)(words[offs])

#define YUNIWORD_REF_PTR(words,offs) \
    (void*)(uintptr_t)(words[offs])

#define YUNIWORD_REF_DOUBLE(words,offs) \
    (*((double *)(char*)(&words[offs])))

#define YUNIWORD_SET_SINT(words,offs,v) \
    (*(int64_t*)(&words[offs]) = (int64_t)v)

#define YUNIWORD_SET_UINT(words,offs,v) \
    (words[offs] = (uint64_t)v)

#define YUNIWORD_SET_PTR(words,offs,v) \
    (words[offs] = (uint64_t)(uintptr_t)v)

#define YUNIWORD_SET_DOUBLE(words,offs,v) \
    (*((double *)(char*)(&words[offs])) = (double)v)

#endif /* YUNIFFI_ABIV0_YUNIWORD_H */

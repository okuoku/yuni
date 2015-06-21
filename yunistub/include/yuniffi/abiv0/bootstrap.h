#ifndef YUNIFFI_ABIV0_BOOTSTRAP_H
#define YUNIFFI_ABIV0_BOOTSTRAP_H

/* malloc(0:func 1:size => 0:ptr/null) */
#define YUNIBOOTSTRAP0_MALLOC 1
/* free(0:func 1:ptr =>) */
#define YUNIBOOTSTRAP0_FREE 2
/* dlopen(0:func 1:nameptr => 0:error 1:ptr) */
#define YUNIBOOTSTRAP0_DLOPEN 3
/* dlsym(0:func 1:ptr 2:nameptr => 0:error 1:ptr) */
#define YUNIBOOTSTRAP0_DLSYM 4

#endif

/*
 * yuniffi.c
 */

#include "yuniffi.h"

/*
 * The following function is a dummy one; replace it for
 * your C function definitions.
 */

ScmObj test_yuniffi(void)
{
    return SCM_MAKE_STR("yuniffi is working");
}

/*
 * Module initialization function.
 */
extern void Scm_Init_yuniffilib(ScmModule*);

void Scm_Init_yuniffi(void)
{
    ScmModule *mod;

    /* Register this DSO to Gauche */
    SCM_INIT_EXTENSION(yuniffi);

    /* Create the module if it doesn't exist yet. */
    mod = SCM_MODULE(SCM_FIND_MODULE("yuniffi", TRUE));

    /* Register stub-generated procedures */
    Scm_Init_yuniffilib(mod);
}

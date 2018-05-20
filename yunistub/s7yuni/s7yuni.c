#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "s7.h"

int
main(int ac, char** av){
    int i;
    int add_default_loadpath = 1;
    s7_scheme* s7;
    s7_pointer cur;
    s7 = s7_init();

    /* Check for -NOSTDLIBPATH */
    if(ac > 1){
        if(! strncmp("-NOSTDLIBPATH", av[1], sizeof("-NOSTDLIBPATH"))){
            add_default_loadpath = 0;
        }
    }

    if(add_default_loadpath){
        s7_add_to_load_path(s7, S7YUNI_DEFAULT_LOAD_PATH);
    }

    /* Generate and bind *command-line* */
    cur = s7_nil(s7);
    for(i = 0; i != ac; i++){
        cur = s7_cons(s7, s7_make_string(s7, av[ac-i-1]), cur);
    }
    (void) s7_define_variable(s7, "*command-line*", cur);

    s7_load(s7, "s7yuniboot.scm");
    return 0;
}

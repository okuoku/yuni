set(expected_failures

    #############################################
    ## To Do
    #############################################

    # no 2 arg log
    inexact1-MIT_SCHEME
    inexact1-SCM

    # Exceptions https://github.com/okuoku/yuni/issues/152
    exp0-BIGLOO
    exp1-BIGLOO
    exp2-BIGLOO
    exp0-SCM
    exp1-SCM
    exp2-SCM

    # SIBR0012, not sure if it allowed
    sibr0012gen-STKLOS

    # 2 arg atan
    inexact2-ICYC # Still -0.0 issue
    inexact2-SCM

    # 0.0 vs. -0.0
    sibr0013-DIGAMMA
    sibr0013-ICYC
    sibr0013-IRON_SCHEME # Reader
    sibr0013-BIGLOO
    sibr0013-S7
    sibr0013-SCM # Reader
    sibr0013-STKLOS

    #############################################
    ## Bugs
    #############################################

    # https://github.com/okuoku/yuni/issues/129
    # https://debbugs.gnu.org/cgi/bugreport.cgi?bug=40584
    iter0-GUILE3

    # Racket: Broken bytevector I/O
    # https://github.com/okuoku/yuni/issues/95
    # https://github.com/racket/r6rs/issues/3
    io0-RACKET

    # https://github.com/okuoku/yuni/issues/128
    fecore0-KAWA

    # https://github.com/okuoku/yuni/issues/118
    fail2-RACKET

    # Syntax
    app-ICYC

    # https://github.com/justinethier/cyclone/issues/379
    io0-ICYC # Wrong character at end of string

    # https://github.com/justinethier/cyclone/issues/490
    hashtables0-ICYC

    # https://github.com/okuoku/yuni/issues/146
    strings0-BIGLOO

    # Exception incompatibilities https://github.com/okuoku/yuni/issues/151
    exp0-ICYC
    exp2-ICYC

    # Parameter incompatibilities https://github.com/okuoku/yuni/issues/153
    exp2-MIT_SCHEME
    exp2-GUILE # Is guile2

    # incompat native string-copy https://github.com/okuoku/yuni/issues/160
    strings0-S7

    # Macro expansion on interactive-environment
    # https://github.com/okuoku/yuni/issues/168
    synrule0-CHICKEN5_CSI
    synrule0-FOMENT
    synrule0-BIGLOO
    synrule0-MIT_SCHEME
    synrule1-BIGLOO
    synrule0-ICYC

    #############################################
    ## Known limitation / Resolved as spec
    #############################################

    # Quasi-quote incompatibility
    # https://github.com/okuoku/yuni/issues/117
    qq1-S7

    # zero values representation
    # https://github.com/okuoku/yuni/issues/167
    values2-S7

    )

set(expected_failures

    #############################################
    ## To Do
    #############################################

    # Exceptions 
    # https://github.com/okuoku/yuni/issues/152
    exp0-BIGLOO
    exp1-BIGLOO
    exp2-BIGLOO
    exp0-SCM
    exp1-SCM
    exp2-SCM

    # (abs -0.0) => -0.0
    # https://github.com/okuoku/yuni/issues/173
    inexact3-CHIBI_SCHEME
    inexact3-DIGAMMA
    inexact3-FOMENT
    inexact3-KAWA
    inexact3-STKLOS

    # SCM do not implement -0.0 printing(SIBR0013)
    # https://github.com/okuoku/yuni/issues/173
    inexact3-SCM

    # IronScheme do not implement -0.0(SIBR0013)
    # https://github.com/okuoku/yuni/issues/173
    inexact3-IRON_SCHEME

    #############################################
    ## Bugs
    #############################################

    # https://debbugs.gnu.org/cgi/bugreport.cgi?bug=40584
    # https://github.com/okuoku/yuni/issues/129
    iter0-GUILE3

    # https://github.com/justinethier/cyclone/issues/379
    io0-ICYC # Wrong character at end of string

    # https://github.com/justinethier/cyclone/issues/490
    hashtables0-ICYC

    # https://github.com/okuoku/yuni/issues/146
    strings0-BIGLOO

    # Parameter incompatibilities 
    # https://github.com/okuoku/yuni/issues/153
    exp2-MIT_SCHEME
    exp2-GUILE # Is guile2
    exp0-ICYC
    exp2-ICYC

    # Macro expansion on interactive-environment
    # https://github.com/okuoku/yuni/issues/168
    app-ICYC
    synrule0-CHICKEN5_CSI
    synrule0-FOMENT
    synrule0-BIGLOO
    synrule0-MIT_SCHEME
    synrule1-BIGLOO
    synrule0-ICYC

    #############################################
    ## Known limitation / Resolved as spec
    #############################################

    # https://github.com/okuoku/yuni/issues/128
    fecore0-KAWA

    # Racket: (exit #f) is broken
    # https://github.com/racket/r6rs/issues/9
    # https://github.com/okuoku/yuni/issues/118
    fail2-RACKET

    # Racket: Broken bytevector I/O
    # https://github.com/racket/r6rs/issues/3
    # https://github.com/okuoku/yuni/issues/95
    io0-RACKET

    # Quasi-quote incompatibility
    # https://github.com/okuoku/yuni/issues/117
    qq1-S7

    # zero values representation
    # https://github.com/okuoku/yuni/issues/167
    values2-S7 # SIBR0014
    err-sibr0014-S7

    # STKlos do not allow (read-string 0 <port>)
    # https://github.com/okuoku/yuni/issues/132
    sibr0012gen-STKLOS
    err-sibr0012string-STKLOS

    )

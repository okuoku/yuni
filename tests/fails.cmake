set(expected_failures

    #############################################
    ## To Do
    #############################################

    # R7RS list-copy
    core2-MIT_SCHEME

    # no 2 arg log
    inexact1-MIT_SCHEME
    inexact1-STKLOS
    inexact1-SCM

    # SIBR0012, not sure if it allowed
    sibr0012gen-STKLOS

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

    # https://github.com/okuoku/yuni/issues/140
    iter0-IRON_SCHEME

    # Syntax
    miniread0-ICYC
    app-ICYC

    # https://github.com/justinethier/cyclone/issues/379
    io0-ICYC

    # hashtable
    hashtables0-ICYC

    # https://github.com/okuoku/yuni/issues/146
    strings0-BIGLOO

    #############################################
    ## Known limitation / Resolved as spec
    #############################################

    # Quasi-quote incompatibility
    # https://github.com/okuoku/yuni/issues/117
    qq1-S7
    )

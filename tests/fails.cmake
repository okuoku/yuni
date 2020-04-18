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

    # STKLOS: Bug [(read-bytevector! bv2 p 0 3)]  Expected: 2  Actual: 3
    # https://github.com/okuoku/yuni/issues/133
    io0-STKLOS

    # https://github.com/okuoku/yuni/issues/128
    fecore0-KAWA

    # https://github.com/okuoku/yuni/issues/118
    fail2-RACKET

    #############################################
    ## Known limitation / Resolved as spec
    #############################################

    # Quasi-quote incompatibility
    # https://github.com/okuoku/yuni/issues/117
    qq1-KAWA
    qq1-S7
    qq1-SCM

    )

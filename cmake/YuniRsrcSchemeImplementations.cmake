##
## GUILE
##    guile -l lib-runtime/guile/guile-load.scm -L $YUNIMOD -L lib-r6rs
##          -L lib-stub/guile -L lib-stub/r6rs-common -L lib-stub/gen
##          -L lib -L lib-compat -L lib-compat/yuni-srfi
##
set(YUNIIMPL_GUILE_LIBS
    lib-r6rs
    lib-stub/guile
    lib-stub/r6rs-common
    lib-stub/gen
    lib
    lib-compat
    lib-compat-srfi)

##
## GAUCHE
##    gosh -r7 -I lib-runtime/gauche -I lib-runtime/r7rs -I lib-stub/gauche
##         -A $ROOT
##
set(YUNIIMPL_GAUCHE_LIBS
    lib-runtime/gauche
    lib-runtime/r7rs
    lib-stub/gauche)

##
## SAGITTARIUS
##    sagittarius --loadpath=lib-r6rs --loadpath=lib --loadpath=$YUNIMOD 
##                --loadpath=lib-runtime/r7rs
##                --loadpath=lib-stub/r6rs-common
##                --loadpath=lib-stub/sagittarius --loadpath=lib-stub/gen
##                --loadpath=lib-compat 
##
set(YUNIIMPL_SAGITTARIUS_LIBS
    # Top-is-least-significant
    lib-r6rs
    lib
    lib-runtime/r7rs
    lib-stub/r6rs-common
    lib-stub/sagittarius
    lib-stub/gen
    lib-compat)

##
## CHIBI-SCHEME
##    chibi-scheme -I$YUNIMOD -I lib-stub/chibi -I lib-runtime/r7rs
##                 -I lib-runtime/chibi
##
set(YUNIIMPL_CHIBI_SCHEME_LIBS
    lib-stub/chibi
    lib-runtime/r7rs
    lib-runtime/chibi)

##
## RACKET
##    racket -I scheme/init -l- r6rs/run.rkt ++path lib-runtime/racket
##           ++path lib-stub/racket
##
set(YUNIIMPL_RACKET_LIBS
    lib-runtime/racket
    lib-stub/racket)

##
## VICARE
##    vicare --source-path $YUNIMOD --source-path lib-r6rs
##           --source-path lib-stub/vicare --source-path lib-stub/r6rs-common
##           --source-path lib-compat --source-path lib
##
set(YUNIIMPL_VICARE_LIBS
    lib-r6rs
    lib-stub/vicare
    lib-stub/r6rs-common
    lib-compat
    lib)

##
## CHICKEN
##    csi -b -require-extension r7rs lib-runtime/r7rs/yuniloader-csi.scm
##

## 
## NMOSH
##     nmosh --loadpath=$YUNIMOD:lib-runtime/nmosh:lib-r6rs:
##             lib-stub/r6rs-common:lib:lib-compat:lib-compat/yuni-srfi:
##             lib-stub/nmosh:lib-stub/gen
##
set(YUNIIMPL_NMOSH_LIBS
    lib-runtime/nmosh
    lib-stub/nmosh
    lib-r6rs
    lib-stub/r6rs-common
    lib
    lib-compat
    lib-compat/yuni-srfi
    lib-stub/gen)


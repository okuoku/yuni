##
## GUILE
##   Uses Racket generator and internal-paste
##
set(YUNIIMPL_GUILE_BOOTLIBS
    lib-runtime/guile)

##
## GAUCHE
##    gosh -r7 -I lib-runtime/gauche -I lib-runtime/r7rs -I lib-stub/gauche
##         -A $ROOT
##
set(YUNIIMPL_GAUCHE_BOOTLIBS
    lib-runtime/gauche
    lib-runtime/r7rs)

##
## SAGITTARIUS
##    sagittarius --loadpath=lib-r6rs --loadpath=lib --loadpath=$YUNIMOD 
##                --loadpath=lib-runtime/r7rs
##                --loadpath=lib-stub/r6rs-common
##                --loadpath=lib-stub/sagittarius --loadpath=lib-stub/gen
##                --loadpath=lib-compat 
##
set(YUNIIMPL_SAGITTARIUS_BOOTLIBS
    lib-runtime/r7rs)

##
## CHIBI-SCHEME
##    chibi-scheme -I$YUNIMOD -I lib-stub/chibi -I lib-runtime/r7rs
##                 -I lib-runtime/chibi
##
set(YUNIIMPL_CHIBI_SCHEME_BOOTLIBS
    lib-runtime/r7rs
    lib-runtime/chibi
    # Chibi-scheme will search for include files for these directories
    # These are included from lib-stub
    lib
    lib-compat)

##
## RACKET
##    racket -I scheme/init -l- r6rs/run.rkt ++path lib-runtime/racket
##           ++path lib-stub/racket
##
set(YUNIIMPL_RACKET_BOOTLIBS
    lib-runtime/racket)

##
## VICARE
##    vicare --source-path $YUNIMOD --source-path lib-r6rs
##           --source-path lib-stub/vicare --source-path lib-stub/r6rs-common
##           --source-path lib-compat --source-path lib 
##           --source-path lib-stub/gen
##
set(YUNIIMPL_VICARE_BOOTLIBS
    lib-r6rs
    lib-compat
    lib)

##
## CHICKEN
##
set(YUNIIMPL_CHICKEN_BOOTLIBS
    lib-runtime/r7rs)

## 
## NMOSH
##     nmosh --loadpath=$YUNIMOD:lib-runtime/nmosh:lib-r6rs:
##             lib-stub/r6rs-common:lib:lib-compat:lib-compat/yuni-srfi:
##             lib-stub/nmosh:lib-stub/gen
##
set(YUNIIMPL_NMOSH_BOOTLIBS
    lib-runtime/nmosh
    lib-r6rs
    lib-compat
    lib)

##
## KAWA
##
# Nothing to do for kawa - single import directory

##
## LARCENY
##     larceny -r7r6 -path ./lib-stub/larceny:./lib:./lib-compat 
set(YUNIIMPL_LARCENY_BOOTLIBS
    lib-r6rs
    lib-compat
    lib)

##
## Chez Scheme
##     scheme-script.exe --libdirs lib-r6rs;lib-stub/chez;
##                       lib-stub/r6rs-common;lib-compat;
##                       lib;lib-compat/yuni-srfi
set(YUNIIMPL_CHEZ_SCHEME_BOOTLIBS
    lib-r6rs
    lib-compat
    lib)

##
## Rapid-gambit
##    rapid-gambit -I lib-stub/rapid-gambit -I lib-runtime/r7rs
##
set(YUNIIMPL_RAPID_GAMBIT_BOOTLIBS
    lib-runtime/r7rs)

##
## Gambit
##
set(YUNIIMPL_GAMBIT_BOOTLIBS
    lib-compat
    lib)

##
## MIT_SCHEME
##
set(YUNIIMPL_MIT_SCHEME_BOOTLIBS
    lib-compat
    lib)

##
## IRON_SCHEME
##
set(YUNIIMPL_IRON_SCHEME_BOOTLIBS
    lib-r6rs
    lib-compat
    lib)

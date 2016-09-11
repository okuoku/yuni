##
## GUILE
##   Uses Racket generator and internal-paste
##
set(YUNIIMPL_GUILE_BOOTLIBS
    lib-runtime/guile)

set(YUNIIMPL_GUILE_LIBS
    lib-runtime/guile
    lib-stub/guile
    lib-r6rs
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
set(YUNIIMPL_GAUCHE_BOOTLIBS
    lib-runtime/gauche
    lib-runtime/r7rs)

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
set(YUNIIMPL_SAGITTARIUS_BOOTLIBS
    # Top-is-least-significant
    lib-runtime/r7rs)

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
set(YUNIIMPL_CHIBI_SCHEME_BOOTLIBS
    lib-runtime/r7rs
    lib-runtime/chibi
    # Chibi-scheme will search for include files for these directories
    # These are included from lib-stub
    lib
    lib-compat)
set(YUNIIMPL_CHIBI_SCHEME_LIBS
    lib-stub/chibi
    lib-runtime/r7rs
    lib-runtime/chibi)

##
## RACKET
##    racket -I scheme/init -l- r6rs/run.rkt ++path lib-runtime/racket
##           ++path lib-stub/racket
##
set(YUNIIMPL_RACKET_BOOTLIBS
    lib-runtime/racket)

set(YUNIIMPL_RACKET_LIBS
    lib-runtime/racket
    lib-stub/racket)

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

set(YUNIIMPL_VICARE_LIBS
    lib-r6rs
    lib-stub/vicare
    lib-stub/r6rs-common
    lib-compat
    lib
    lib-stub/gen)

##
## CHICKEN
##
set(YUNIIMPL_CHICKEN_BOOTLIBS
    lib-runtime/r7rs)

set(YUNIIMPL_MIT_SCHEME_LIBS)

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

set(YUNIIMPL_NMOSH_LIBS
    lib-runtime/nmosh
    lib-stub/nmosh
    lib-r6rs
    lib-stub/r6rs-common
    lib
    lib-compat
    lib-compat/yuni-srfi
    lib-stub/gen)

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

set(YUNIIMPL_LARCENY_LIBS
    lib-stub/larceny
    lib-stub/gen
    lib
    lib-r6rs
    lib-compat)

##
## Chez Scheme
##     scheme-script.exe --libdirs lib-r6rs;lib-stub/chez;
##                       lib-stub/r6rs-common;lib-compat;
##                       lib;lib-compat/yuni-srfi
set(YUNIIMPL_CHEZ_SCHEME_BOOTLIBS
    lib-r6rs
    lib-compat
    lib)

set(YUNIIMPL_CHEZ_SCHEME_LIBS
    lib-stub/chez
    lib-stub/r6rs-common
    lib-r6rs
    lib-compat
    lib
    lib-compat/yuni-srfi
    lib-stub/gen)

##
## Rapid-gambit
##    rapid-gambit -I lib-stub/rapid-gambit -I lib-runtime/r7rs
##
set(YUNIIMPL_RAPID_GAMBIT_BOOTLIBS
    lib-runtime/r7rs)

set(YUNIIMPL_RAPID_GAMBIT_LIBS
    lib-stub/rapid-gambit
    lib-runtime/r7rs)

##
## Gambit
##
set(YUNIIMPL_GAMBIT_BOOTLIBS
    lib-compat
    lib)

set(YUNIIMPL_GAMBIT_LIBS
    lib-stub/gambit
    lib
    lib-compat
    lib-stub/gen)

##
## MIT_SCHEME
##
set(YUNIIMPL_MIT_SCHEME_BOOTLIBS
    lib-compat
    lib)

set(YUNIIMPL_MIT_SCHEME_LIBS
    lib-stub/mit-scheme
    lib
    lib-compat
    lib-stub/gen)

##
## IRON_SCHEME
##
set(YUNIIMPL_IRON_SCHEME_BOOTLIBS
    lib-r6rs
    lib-compat
    lib)

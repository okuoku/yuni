# Generate yunibase implementations runners
#
# INPUTs:
#   YUNI_BASEDIR = repository top
#   YUNI_WITH_YUNIBASE = basepath for yunibase build
#   YUNIBASE_VANILLA_PATH = Output for "vanilla" runner


include(YuniRsrcSchemeImplementations)
include(YuniRsrcSchemeRunners)
include(YuniWinSupport)

# Output vanilla/*.sh
function(emit_tmpl_runner_sh outpath ldpathname ldpath addpath execpath args)
    # args = a string for additional args
    # FIXME: SCHEMEHEAPDIRS is ChezScheme specific
    file(WRITE 
        "${outpath}"
        "#!/bin/sh\nexport SCHEMEHEAPDIRS=${ldpath}/csv%v/%m\nexport PATH=\"${addpath}:\$PATH\"\nexport ${ldpathname}=${ldpath}:\$${ldpathname}\nexec ${execpath} ${args} \$*\n")
    execute_process(
        COMMAND chmod +x ${outpath}
        )
endfunction()

# FIXME: Change this for MacOS Mach-O
set(_ldpathname LD_LIBRARY_PATH)

function(emit_vanilla_runner_sh0 flav impl cmd cmdname)
    # flav = stable | current
    file(MAKE_DIRECTORY ${YUNIBASE_VANILLA_PATH})
    emit_tmpl_runner_sh(${YUNIBASE_VANILLA_PATH}/${cmdname}
        ${_ldpathname}
        ${YUNI_WITH_YUNIBASE}/${flav}/${impl}/lib
        ${YUNI_WITH_YUNIBASE}/${flav}/${impl}/bin
        ${YUNI_WITH_YUNIBASE}/${flav}/${impl}/bin/${cmd}
        "")
endfunction()

function(emit_vanilla_runner_sh_icyc flav impl cmd)
    # flav = stable | current
    file(MAKE_DIRECTORY ${YUNIBASE_VANILLA_PATH})
    emit_tmpl_runner_sh(${YUNIBASE_VANILLA_PATH}/${cmd}
        ${_ldpathname}
        ${YUNI_WITH_YUNIBASE}/${flav}/${impl}/lib
        ${YUNI_WITH_YUNIBASE}/${flav}/${impl}/bin
        ${YUNI_WITH_YUNIBASE}/${flav}/${impl}/bin/${cmd}
        "-A ${YUNI_WITH_YUNIBASE}/${flav}/${impl}/share/cyclone")
endfunction()

function(emit_vanilla_runner_sh flav impl cmd)
    emit_vanilla_runner_sh0(${flav} ${impl} ${cmd} ${cmd})
endfunction()

macro(gen_string_args var)
    set(${var} "")
    foreach(e ${ARGN})
        set(${var} "${${var}} ${e}")
    endforeach()
endmacro()

macro(gen_yunilibpaths var)
    set(${var})
    foreach(e ${ARGN})
        list(APPEND ${var} "${YUNI_BASEDIR}/${e}")
    endforeach()
endmacro()

macro(yunibase_check_implementations)
    # chibi-scheme
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/chibi-scheme/bin/chibi-scheme)
        set(YUNIBASE_HAVE_CHIBI_SCHEME_CURRENT 1)
    endif()
    # gauche
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/gauche/bin/gosh)
        set(YUNIBASE_HAVE_GAUCHE_CURRENT 1)
    endif()
    # guile2
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/guile/bin/guile)
        set(YUNIBASE_HAVE_GUILE_CURRENT 1)
    endif()
    # guile3
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/guile3/bin/guile)
        set(YUNIBASE_HAVE_GUILE3_CURRENT 1)
    endif()
    # racket
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/racket/bin/racket)
        set(YUNIBASE_HAVE_RACKET_CURRENT 1)
    endif()
    # sagittarius
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/sagittarius/bin/sagittarius)
        set(YUNIBASE_HAVE_SAGITTARIUS_CURRENT 1)
    endif()
    # chicken
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/chicken/bin/chicken)
        set(YUNIBASE_HAVE_CHICKEN_CURRENT 1)
    endif()
    # chicken5
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/chicken5/bin/chicken)
        set(YUNIBASE_HAVE_CHICKEN5_CURRENT 1)
    endif()
    # Vicare
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/vicare/bin/vicare)
        set(YUNIBASE_HAVE_VICARE_CURRENT 1)
    endif()
    # nmosh
    if(EXISTS ${YUNI_WITH_YUNIBASE}/stable/nmosh/bin/nmosh)
        set(YUNIBASE_HAVE_NMOSH_STABLE 1)
    endif()
    # kawa
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/kawa/bin/kawa) # UNIX only
        set(YUNIBASE_HAVE_KAWA_CURRENT 1)
    endif()
    # Larceny
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/larceny/bin/larceny)
        set(YUNIBASE_HAVE_LARCENY_CURRENT 1)
    endif()
    # Chez Scheme
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/chez/bin/scheme)
        set(YUNIBASE_HAVE_CHEZ_SCHEME_CURRENT 1)
    endif()
    # Rapid-gambit
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/rapid-gambit/bin/rapid-gambit)
        set(YUNIBASE_HAVE_RAPID_GAMBIT_CURRENT 1)
    endif()
    # Picrin
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/picrin/bin/picrin)
        set(YUNIBASE_HAVE_PICRIN_CURRENT 1)
    endif()
    # Gambit(GSI)
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/gambit/bin/gsi)
        set(YUNIBASE_HAVE_GAMBIT_CURRENT 1)
    endif()
    # MIT/GNU Scheme
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/mit-scheme/bin/mit-scheme)
        set(YUNIBASE_HAVE_MIT_SCHEME_CURRENT 1)
    endif()
    # s7
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/s7yuni/bin/s7yuni)
        set(YUNIBASE_HAVE_S7_CURRENT 1)
    endif()
    # SCM (with SLIB)
    if(EXISTS ${YUNI_WITH_YUNIBASE}/stable/scm/bin/scm)
        set(YUNIBASE_HAVE_SCM_STABLE 1)
    endif()
    # Cyclone
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/cyclone/bin/icyc)
        set(YUNIBASE_HAVE_CYCLONE_CURRENT 1)
    endif()
    # STklos
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/stklos/bin/stklos)
        set(YUNIBASE_HAVE_STKLOS_CURRENT 1)
    endif()
    # Digamma
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/digamma/bin/digamma)
        set(YUNIBASE_HAVE_DIGAMMA_CURRENT 1)
    endif()
    # Foment
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/foment/bin/foment)
        set(YUNIBASE_HAVE_FOMENT_CURRENT 1)
    endif()
endmacro()

# Emit actual runners
function(emit_yunibase_runners)
    yunibase_check_implementations()
    if(YUNI_WITH_YUNIBASE AND YUNIBASE_VANILLA_PATH)
        if(YUNIBASE_HAVE_CHIBI_SCHEME_CURRENT)
            emit_vanilla_runner_sh(current chibi-scheme chibi-scheme)
            emit_vanilla_runner_sh(current chibi-scheme chibi-ffi)
        endif()
        if(YUNIBASE_HAVE_GAUCHE_CURRENT)
            emit_vanilla_runner_sh(current gauche gosh)
            emit_vanilla_runner_sh(current gauche gauche-config)
            emit_vanilla_runner_sh(current gauche gauche-package)
        endif()
        if(YUNIBASE_HAVE_GUILE_CURRENT)
            emit_vanilla_runner_sh(current guile guile)
        endif()
        if(YUNIBASE_HAVE_GUILE3_CURRENT)
            emit_vanilla_runner_sh0(current guile3 guile guile3)
        endif()
        if(YUNIBASE_HAVE_RACKET_CURRENT)
            emit_vanilla_runner_sh(current racket racket)
            emit_vanilla_runner_sh(current racket raco)
        endif()
        if(YUNIBASE_HAVE_SAGITTARIUS_CURRENT)
            emit_vanilla_runner_sh(current sagittarius sagittarius)
        endif()
        #if(YUNIBASE_HAVE_CHICKEN_CURRENT)
        #    emit_vanilla_runner_sh(current chicken csi)
        #    emit_vanilla_runner_sh(current chicken csc)
        #    emit_vanilla_runner_sh(current chicken chicken)
        #endif()
        if(YUNIBASE_HAVE_CHICKEN5_CURRENT)
            emit_vanilla_runner_sh(current chicken5 csi)
            emit_vanilla_runner_sh(current chicken5 csc)
            emit_vanilla_runner_sh(current chicken5 chicken)
        endif()
        if(YUNIBASE_HAVE_NMOSH_STABLE)
            emit_vanilla_runner_sh(stable nmosh nmosh)
        endif()
        if(YUNIBASE_HAVE_LARCENY_CURRENT)
            emit_vanilla_runner_sh(current larceny larceny.bin)
        endif()
        if(YUNIBASE_HAVE_CHEZ_SCHEME_CURRENT)
            emit_vanilla_runner_sh0(current chez scheme chez-scheme)
            emit_vanilla_runner_sh0(current chez petite petite-chez-scheme)
        endif()
        if(YUNIBASE_HAVE_RAPID_GAMBIT_CURRENT)
            emit_vanilla_runner_sh(current rapid-gambit rapid-gambit)
        endif()
        if(YUNIBASE_HAVE_PICRIN_CURRENT)
            emit_vanilla_runner_sh(current picrin picrin)
        endif()
        if(YUNIBASE_HAVE_GAMBIT_CURRENT)
            emit_vanilla_runner_sh(current gambit gsc)
            emit_vanilla_runner_sh(current gambit gsi)
        endif()
        if(YUNIBASE_HAVE_MIT_SCHEME_CURRENT)
            emit_vanilla_runner_sh(current mit-scheme mit-scheme)
        endif()
        if(YUNIBASE_HAVE_S7_CURRENT)
            emit_vanilla_runner_sh(current s7yuni s7yuni)
        endif()
        if(YUNIBASE_HAVE_KAWA_CURRENT)
            emit_vanilla_runner_sh(current kawa kawa)
        endif()
        if(YUNIBASE_HAVE_SCM_STABLE)
            emit_vanilla_runner_sh(stable scm scm)
        endif()
        if(YUNIBASE_HAVE_CYCLONE_CURRENT)
            emit_vanilla_runner_sh(current cyclone cyclone)
            emit_vanilla_runner_sh_icyc(current cyclone icyc)
        endif()
        if(YUNIBASE_HAVE_STKLOS_CURRENT)
            emit_vanilla_runner_sh(current stklos stklos)
        endif()
        if(YUNIBASE_HAVE_DIGAMMA_CURRENT)
            emit_vanilla_runner_sh(current digamma digamma)
        endif()
        if(YUNIBASE_HAVE_FOMENT_CURRENT)
            emit_vanilla_runner_sh(current foment foment)
        endif()
    endif()
endfunction()

# Generate yunibase implementations runners
#
# INPUTs:
#   YUNI_BASEDIR = repository top
#   YUNI_WITH_YUNIBASE = basepath for yunibase build
#   YUNIBASE_VANILLA_PATH = Output for "vanilla" runner
#   YUNIBASE_YUNIFIED_PATH = Output for "yunified" runner (for post-bootstrap)


include(YuniRsrcSchemeImplementations)
include(YuniRsrcSchemeRunners)

# Output vanilla/*.sh
function(emit_tmpl_runner_sh outpath ldpathname ldpath addpath execpath args)
    # args = a string for additional args
    # FIXME: SCHEMEHEAPDIRS is ChezScheme specific
    file(WRITE 
        "${outpath}"
        "#!/bin/sh\nexport SCHEMEHEAPDIRS=${ldpath}/csv%v/%m\nexport PATH=${addpath}:\$PATH\nexport ${ldpathname}=${ldpath}:\$${ldpathname}\nexec ${execpath} ${args} \$*\n")
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

function(emit_vanilla_vicare_runner_sh flav impl cmd)
    # WAR: Add --library-path ${libpath}/vicare-scheme
    set(_argsstr "--library-path ${YUNI_WITH_YUNIBASE}/${flav}/${impl}/lib/vicare-scheme")
    file(MAKE_DIRECTORY ${YUNIBASE_VANILLA_PATH})
    emit_tmpl_runner_sh(${YUNIBASE_VANILLA_PATH}/${cmd}
        ${_ldpathname}
        ${YUNI_WITH_YUNIBASE}/${flav}/${impl}/lib
        ${YUNI_WITH_YUNIBASE}/${flav}/${impl}/bin
        ${YUNI_WITH_YUNIBASE}/${flav}/${impl}/bin/${cmd}
        "${_argsstr}")
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
    # guile
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/guile/bin/guile)
        set(YUNIBASE_HAVE_GUILE_CURRENT 1)
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
    # Vicare
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/vicare/bin/vicare)
        set(YUNIBASE_HAVE_VICARE_CURRENT 1)
    endif()
    # nmosh
    if(EXISTS ${YUNI_WITH_YUNIBASE}/stable/nmosh/bin/nmosh)
        set(YUNIBASE_HAVE_NMOSH_STABLE 1)
    endif()
    # kawa
    if(EXISTS ${YUNI_WITH_YUNIBASE}/current/kawa/kawa.jar)
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
        if(YUNIBASE_HAVE_RACKET_CURRENT)
            emit_vanilla_runner_sh(current racket racket)
            emit_vanilla_runner_sh(current racket raco)
        endif()
        if(YUNIBASE_HAVE_SAGITTARIUS_CURRENT)
            emit_vanilla_runner_sh(current sagittarius sagittarius)
        endif()
        if(YUNIBASE_HAVE_CHICKEN_CURRENT)
            emit_vanilla_runner_sh(current chicken csi)
            emit_vanilla_runner_sh(current chicken csc)
            emit_vanilla_runner_sh(current chicken chicken)
        endif()
        if(YUNIBASE_HAVE_VICARE_CURRENT)
            emit_vanilla_vicare_runner_sh(current vicare vicare)
        endif()
        if(YUNIBASE_HAVE_NMOSH_STABLE)
            emit_vanilla_runner_sh(stable nmosh nmosh)
        endif()
        if(YUNIBASE_HAVE_LARCENY_CURRENT)
            emit_vanilla_runner_sh(current larceny larceny)
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

    endif()
endfunction()

#
# "Yunified" script generation
#
# FIXME: Move below outside of Yunibase
#

function(emit_tmpl_runwitharg_cmd outpath execpath args)
    file(WRITE "${outpath}.bat"
        "\"${execpath}\" ${args} %*\n")
endfunction()

function(emit_tmpl_runwitharg_sh outpath execpath args)
    # args = a string for additional args
    file(WRITE "${outpath}"
        "#!/bin/sh\n\nexec ${execpath} ${args} \$*\n")
    execute_process(COMMAND chmod +x ${outpath})
endfunction()

function(emit_yuniboot_kawa_runner)
    if(YUNI_KAWA_JAR AND Java_JAVA_EXECUTABLE)
        file(MAKE_DIRECTORY ${YUNI_YUNIBOOT_PATH})
        gen_stubprefix(stub yuniboot kawa)
        set(yuniloader ${YUNI_BASEDIR}/yuniloader/yuniloader-kawa.scm)
        if(WIN32)
            yuni_path_chop_drive(yuniloader ${yuniloader})
            yuni_path_chop_drive(stub ${stub})
        endif()

        set(kawa_args
            "-classpath ${YUNI_KAWA_JAR} kawa.repl --r7rs ${yuniloader} -I ${stub}")
        if(WIN32)
            emit_tmpl_runwitharg_cmd(
                ${YUNI_YUNIBOOT_PATH}/kawa
                ${Java_JAVA_EXECUTABLE} ${kawa_args})
            emit_tmpl_runwitharg_cmd(
                ${YUNIBASE_YUNIFIED_PATH}/kawa
                ${Java_JAVA_EXECUTABLE} ${kawa_args})
        else()
            emit_tmpl_runwitharg_sh(
                ${YUNI_YUNIBOOT_PATH}/kawa
                ${Java_JAVA_EXECUTABLE} ${kawa_args})
            emit_tmpl_runwitharg_sh(
                ${YUNIBASE_YUNIFIED_PATH}/kawa
                ${Java_JAVA_EXECUTABLE} ${kawa_args})
        endif()
    endif()
endfunction()

function(emit_yunirunner flav varname cmdvar cmdname)
    if(${cmdvar}) # Check for vanilla Scheme
        set(cmd ${${cmdvar}})
        file(MAKE_DIRECTORY ${YUNI_YUNIBOOT_PATH})
        if(${flav} STREQUAL yuniboot)
            gen_yunilibpaths(_yunilibs ${YUNIIMPL_${varname}_BOOTLIBS})
            set(out ${YUNI_YUNIBOOT_PATH}/${cmdname})
        else()
            set(_yunilibs)
            set(out ${YUNIBASE_YUNIFIED_PATH}/${cmdname})
        endif()
        set(_libs ${YUNI_PLATFORM_LIBDIR} ${_yunilibs})
        calc_impl_yuniboot_commandline(_args ${flav}
            ${varname} ${YUNI_BASEDIR} ${_libs})
        gen_string_args(_argsstr ${_args})
        if(WIN32)
            # Workaround for MIT_SCHEME Win32 executable
            if(${cmdvar} STREQUAL YUNI_MIT_SCHEME)
                get_filename_component(instdir
                    "${YUNI_MIT_SCHEME}"
                    PATH)
                # https://savannah.gnu.org/bugs/?31710
                set(_argsstr "--heap 512 --library \"${instdir}/../lib\" ${_argsstr}")
            endif()
            emit_tmpl_runwitharg_cmd(${out}
                ${cmd}
                "${_argsstr}")
        else()
            emit_tmpl_runwitharg_sh(${out}
                ${cmd}
                "${_argsstr}")
        endif()
    endif()
endfunction()

function(emit_yuni_runner)
    emit_yunirunner(yuniboot ${ARGN})
    emit_yunirunner(yunified ${ARGN})
endfunction()

function(emit_yuni_runners)
    emit_yuni_runner(CHIBI_SCHEME YUNI_CHIBI_SCHEME chibi-scheme)
    emit_yuni_runner(GAUCHE       YUNI_GOSH         gosh)
    emit_yuni_runner(GUILE        YUNI_GUILE        guile)
    emit_yuni_runner(RACKET       YUNI_RACKET       racket)
    emit_yuni_runner(SAGITTARIUS  YUNI_SAGITTARIUS  sagittarius)
    emit_yuni_runner(CHICKEN      YUNI_CHICKEN_CSI  csi)
    emit_yuni_runner(VICARE       YUNI_VICARE       vicare)
    emit_yuni_runner(NMOSH        YUNI_NMOSH        nmosh)
    emit_yuni_runner(LARCENY      YUNI_LARCENY      larceny)
    emit_yuni_runner(CHEZ_SCHEME  YUNI_CHEZ_SCHEME  chez-scheme)
    emit_yuni_runner(CHEZ_SCHEME  YUNI_CHEZ_PETITE  petite-chez-scheme)
    emit_yuni_runner(RAPID_GAMBIT YUNI_RAPID_GAMBIT rapid-gambit)
    emit_yuni_runner(PICRIN       YUNI_PICRIN       picrin)
    emit_yuni_runner(GAMBIT       YUNI_GSI          gsi)
    emit_yuni_runner(MIT_SCHEME   YUNI_MIT_SCHEME   mit-scheme)
    emit_yuni_runner(IRON_SCHEME  YUNI_IRON_SCHEME  ironscheme)
    emit_yuniboot_kawa_runner()
endfunction()

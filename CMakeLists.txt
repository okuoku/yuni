#
# New Bootstrap
#


cmake_minimum_required(VERSION 3.12)
project(yuni NONE) # Only for selfboot scripts

include(CTest)

set(YUNIROOT ${CMAKE_CURRENT_LIST_DIR})
get_filename_component(YUNIROOT ${YUNIROOT} ABSOLUTE)
include(${YUNIROOT}/config/implinfo.cmake)
set(BOOTDIR ${CMAKE_CURRENT_BINARY_DIR})

list(APPEND CMAKE_MODULE_PATH
    # Private modules
    ${YUNIROOT}/cmake
    # Public modules
    ${YUNIROOT}/cmake/public)

if(YUNI_WITH_YUNIBASE)
    include(YuniBaseGenRunners)
    set(YUNI_BASEDIR ${YUNIROOT})
    if(IS_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/vanilla)
        message(STATUS "Removing existing vanilla directory...")
        file(REMOVE_RECURSE "${CMAKE_CURRENT_BINARY_DIR}/vanilla")
    endif()
    set(YUNIBASE_VANILLA_PATH ${CMAKE_CURRENT_BINARY_DIR}/vanilla)
    set(YUNIBASE_YUNIFIED_PATH ${CMAKE_CURRENT_BINARY_DIR}/bogus)
    emit_yunibase_runners()
endif()

set(VARFILE ${CMAKE_CURRENT_BINARY_DIR}/_vars.cmake)
get_filename_component(VARFILE ${VARFILE} ABSOLUTE)

function(writeproxy scriptname)
    file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/${scriptname}
        "include(${YUNIROOT}/config/implinfo.cmake)\ninclude(${VARFILE})\ninclude(${YUNIROOT}/scripts/${scriptname})\n")
endfunction()

function(writevar varname)
    file(APPEND ${VARFILE} "set(${varname} \"${${varname}}\")\n")
endfunction()

macro(checkimpl nam impl)
    set(_modpath "")
    if(YUNI_${impl})
        writevar(YUNI_${impl})
        list(APPEND impls ${impl})
        if(NOT selfboot_${impl})
            message(FATAL_ERROR "Unknown impl: ${impl}")
        endif()
        if(YUNI_INPLACE_NCCC_MODULES_PREFIX)
            set(_modpath "-MODPATH ${YUNI_INPLACE_NCCC_MODULES_PREFIX}")
        endif()
        if(UNIX)
            set(_cmd "${CMAKE_CURRENT_BINARY_DIR}/run-${nam}.sh")
            set(_runner "${YUNIROOT}/lib-runtime/selfboot/${selfboot_${impl}}")
            if(arg_WAR_LAST_RUNNER_${impl})
                set(_war_runner "${_runner}")
            else()
                set(_war_runner)
            endif()
            if(arg_post_${impl})
                set(_war_argpost ${arg_post_${impl}})
            else()
                set(_war_argpost)
            endif()
            file(WRITE ${_cmd}
                "#!/bin/sh\nexec ${YUNI_${impl}} ${_arg_root} ${arg_ci_${impl}} ${arg_prog_${impl}} ${_runner} ${_war_argpost} ${_modpath} $* ${_war_runner}\n")
            execute_process(
                COMMAND chmod +x ${_cmd})
        endif()
    else()
        message(STATUS "Skip: ${impl}")
    endif()
endmacro()

macro(checkimpls nam cmd impl yuniname)
    set(YUNI_IDENT_${impl} ${yuniname})

    if(YUNI_WITH_YUNIBASE)
        if(NOT YUNI_${impl})
            if(EXISTS ${YUNIBASE_VANILLA_PATH}/${cmd})
                message(STATUS "Yunibase: ${YUNIBASE_VANILLA_PATH}/${cmd}")
                set(YUNI_${impl} ${YUNIBASE_VANILLA_PATH}/${cmd})
            endif()
        else()
            message(STATUS "YUNI_${impl}: ${YUNI_${impl}}")
        endif()
    elseif(YUNI_${impl})
        message(STATUS "YUNI_${impl}: ${YUNI_${impl}}")
    endif()
    checkimpl(${nam} ${impl})

    set(_test ${ARGN})
    if(_test)
        # Call next
        checkimpls(${ARGN})
    else()
        # Term
        if(UNIX)
            set(_cmd "${CMAKE_CURRENT_BINARY_DIR}/run-all.sh")
            file(WRITE ${_cmd}
                "#!/bin/sh\nexec ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/run-all.cmake $*\n")
            execute_process(
                COMMAND chmod +x ${_cmd})
        endif()
    endif()
endmacro()

file(WRITE ${VARFILE} "# Autogenerated. Do not edit.\n")

writevar(YUNIROOT)
writevar(BOOTDIR)
if(YUNI_INPLACE_NCCC_MODULES_PREFIX) # Debug: NCCC module prefix
    writevar(YUNI_INPLACE_NCCC_MODULES_PREFIX)
endif()
writeproxy(run-all.cmake)
writeproxy(run.cmake)

set(impls)

checkimpls(
    # Impl name   Cmd name      CMake Varname   yuniname
    ## R6RS+R7RS
    digamma       digamma       DIGAMMA         digamma
    guile3        guile3        GUILE3          guile  # Currently yunibase only
    sagittarius   sagittarius   SAGITTARIUS     sagittarius
    ## R7RS
    chibi-scheme  chibi-scheme  CHIBI_SCHEME    chibi-scheme
    chicken       csi           CHICKEN5_CSI    chicken
    cyclone       icyc          ICYC            cyclone
    foment        foment        FOMENT          foment
    gauche        gosh          GOSH            gauche
    kawa          kawa          KAWA            kawa
    ## R6RS
    chez          chez-scheme   CHEZ_SCHEME     chez
    guile         guile         GUILE           guile  # Guile2(R6RS only)
    ironscheme    IRONSCHEME    IRON_SCHEME     ironscheme # FIXME:
    racket        racket        RACKET          racket
    ## Generic Scheme
    bigloo        bigloo        BIGLOO          bigloo
    biwascheme    biwasyuni     BIWASYUNI       biwascheme
    gambit        gsi           GSI             gambit
    mit-scheme    mit-scheme    MIT_SCHEME      mit-scheme
    s7            s7yuni        S7              s7
    scm           scm           SCM             scm
    stklos        stklos        STKLOS          stklos
    )

file(APPEND ${VARFILE} "set(YUNIBOOT_IMPLS\n")

foreach(e ${impls})
    file(APPEND ${VARFILE} "    ${e}\n")
endforeach()

file(APPEND ${VARFILE} ")\n")


# Testing
if(NOT YUNI_SKIP_TESTS)
    include(tests/tests.cmake)
endif()


# Generate Nmosh/Sagittarius/Gauche styled loadpath list
macro(gen_loadpath0 var first)
    if(WIN32)
        set(_pathsep ";")
    else()
        set(_pathsep ":")
    endif()
    set(_out ${first})
    foreach(e ${ARGN})
        set(_out "${_out}${_pathsep}${e}")
    endforeach()
    set(${var} "${_out}")
endmacro()

macro(gen_loadpath var)
    gen_loadpath0(_tmp ${ARGN})
    set(${var} "--loadpath=${_tmp}")
endmacro()

macro(gen_platformpath var plt)
    if(${plt} STREQUAL "WIN64")
        set(${var} lib-stub/yunistub-win64)
    elseif(${plt} STREQUAL "WIN32")
        set(${var} lib-stub/yunistub-win32)
    elseif(${plt} STREQUAL "CYGWIN32")
        set(${var} lib-stub/yunistub-cygwin32)
    elseif(${plt} STREQUAL "CYGWIN64")
        set(${var} lib-stub/yunistub-cygwin64)
    else()
        set(${var} lib-stub/yunistub)
    endif()
endmacro()

##
## NMOSH
##

macro(run_nmosh_bootstrap nmosh script root result)
    gen_loadpath(__loadpath_bootstrap
        lib-bootstrap
        lib
        lib-compat)
    execute_process(
        COMMAND "${nmosh}"
        "${__loadpath_bootstrap}" "${script}"
        WORKING_DIRECTORY "${root}"
        RESULT_VARIABLE ${result})
endmacro()

macro(run_nmosh nmosh script root result)
    gen_loadpath(__loadpath_full
        lib-runtime/nmosh
        lib-r6rs
        lib-stub/r6rs-common
        lib
        lib-compat
        lib-compat/yuni-srfi
        lib-stub/nmosh
        lib-stub/gen)
    execute_process(
        COMMAND "${nmosh}"
        "${__loadpath_full}" "${script}"
        WORKING_DIRECTORY "${root}"
        RESULT_VARIABLE ${result})
endmacro()

macro(add_nmosh_test nam platform nmosh script root)
    gen_platformpath(_platform ${platform})
    gen_loadpath(__loadpath_full
        ${_platform}
        lib-runtime/nmosh
        lib-r6rs
        lib-stub/r6rs-common
        lib
        lib-compat
        lib-compat/yuni-srfi
        lib-stub/nmosh
        lib-stub/gen)
    add_test(NAME ${nam}
        COMMAND "${nmosh}"
        "${__loadpath_full}" "${script}"
        WORKING_DIRECTORY "${root}")
endmacro()

# Bootstrap yuni tree
#
# YUNI_BOOTSTRAP_NMOSH : Path to nmosh
# YUNI_BOOTSTRAP_ROOT  : Path to tree root
# YUNI_BOOTSTRAP_STAGE : Install root for nmosh

# set cache directory
set(ENV{NMOSH_CACHEDIR} "${YUNI_BOOTSTRAP_STAGE}/cache")

if(EXISTS "${YUNI_BOOTSTRAP_NMOSH}")
    # Nothing to do
else()
    message(FATAL_ERROR 
        "YUNI_BOOTSTRAP_NMOSH not found [${YUNI_BOOTSTRAP_NMOSH}]")
endif()

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

gen_loadpath(loadpath_bootstrap
    lib-bootstrap
    lib
    lib-compat)

gen_loadpath(loadpath_full
    lib-runtime/nmosh
    lib-r6rs
    lib-stub/r6rs-common
    lib
    lib-compat
    lib-compat/yuni-srfi
    lib-stub/nmosh
    lib-stub/gen)

# Perform bootstrap

# Phase1(Generate yuni API stubs for all implementations)
execute_process(
    COMMAND "${YUNI_BOOTSTRAP_NMOSH}"
    "${loadpath_bootstrap}" scripts/build-nmosh.sps
    WORKING_DIRECTORY "${YUNI_BOOTSTRAP_ROOT}"
    RESULT_VARIABLE rr)

if(rr)
    message(FATAL_ERROR "Failed phase1 (${rr})")
endif()

# Phase2(Generate NCCC API stubs)
execute_process(
    COMMAND "${YUNI_BOOTSTRAP_NMOSH}"
    "${loadpath_full}" scripts/build-apistubs-nmosh.sps
    scripts/build-apistubs-nmosh.sps
    WORKING_DIRECTORY "${YUNI_BOOTSTRAP_ROOT}"
    RESULT_VARIABLE rr)

if(rr)
    message(FATAL_ERROR "Failed phase2 (${rr})")
endif()

# Phase3(Same as phase1)
execute_process(
    COMMAND "${YUNI_BOOTSTRAP_NMOSH}"
    "${loadpath_bootstrap}" scripts/build-nmosh.sps
    WORKING_DIRECTORY "${YUNI_BOOTSTRAP_ROOT}"
    RESULT_VARIABLE rr)

if(rr)
    message(FATAL_ERROR "Failed phase3 (${rr})")
endif()


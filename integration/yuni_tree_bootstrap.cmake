# Bootstrap yuni tree
#
# YUNI_BOOTSTRAP_USE_CHIBI_SCHEME : ON to use chibi-scheme for bootstrap
# YUNI_BOOTSTRAP_CHIBI_SCHEME : Path to chibi-scheme
# YUNI_BOOTSTRAP_NMOSH : Path to nmosh
# YUNI_BOOTSTRAP_ROOT  : Path to tree root
# YUNI_BOOTSTRAP_STAGE : Install root for nmosh

include(YuniRsrcSchemeRunners)

# set cache directory
set(ENV{NMOSH_CACHEDIR} "${YUNI_BOOTSTRAP_STAGE}/cache")

# Select bootstrap
if(YUNI_BOOTSTRAP_USE_CHIBI_SCHEME)
    set(bootstrap_impl "chibi-scheme")
else()
    # Autodetect
    if(EXISTS ${YUNI_BOOTSTRAP_CHIBI_SCHEME})
        set(bootstrap_impl "chibi-scheme")
    else()
        set(bootstrap_impl "nmosh")
    endif()
endif()

if(${bootstrap_impl} STREQUAL "chibi-scheme")
    if(EXISTS "${YUNI_BOOTSTRAP_CHIBI_SCHEME}")
        # Nothing to do
    else()
        message(FATAL_ERROR
            "YUNI_BOOTSTRAP_CHIBI_SCHEME not found [${YUNI_BOOTSTRAP_CHIBI_SCHEME}]")
    endif()
    if(WIN32)
        message(FATAL_ERROR "chibi-scheme requires POSIX")
    endif()
else()
    if(EXISTS "${YUNI_BOOTSTRAP_NMOSH}")
        # Nothing to do
    else()
        message(FATAL_ERROR 
            "YUNI_BOOTSTRAP_NMOSH not found [${YUNI_BOOTSTRAP_NMOSH}]")
    endif()
endif()

macro(do_phase13 nam)
    if(${bootstrap_impl} STREQUAL "chibi-scheme")
        run_chibi_scheme_bootstrap(
            "${YUNI_BOOTSTRAP_CHIBI_SCHEME}" scripts/build-chibi-scheme.sps 
            "${YUNI_BOOTSTRAP_ROOT}" rr)
    else()
        run_nmosh_bootstrap(
            "${YUNI_BOOTSTRAP_NMOSH}" scripts/build-nmosh.sps 
            "${YUNI_BOOTSTRAP_ROOT}" rr)
    endif()
    if(rr)
        message(FATAL_ERROR "Failed ${nam} (${rr})")
    endif()
endmacro()


# Perform bootstrap

# Phase1(Generate yuni API stubs for all implementations)
do_phase13("phase1")

# Phase2(Generate NCCC API stubs)
if(${bootstrap_impl} STREQUAL "chibi-scheme")
    run_chibi_scheme(
        "${YUNI_BOOTSTRAP_CHIBI_SCHEME}" scripts/build-apistubs-nmosh.sps
        "${YUNI_BOOTSTRAP_ROOT}" rr)
else()
    run_nmosh(
        "${YUNI_BOOTSTRAP_NMOSH}" scripts/build-apistubs-nmosh.sps
        "${YUNI_BOOTSTRAP_ROOT}" rr)
endif()

if(rr)
    message(FATAL_ERROR "Failed phase2 (${rr})")
endif()

# Phase3(Same as phase1)
do_phase13("phase3")

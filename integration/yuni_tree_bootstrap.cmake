# Bootstrap yuni tree
#
# YUNI_BOOTSTRAP_NMOSH : Path to nmosh
# YUNI_BOOTSTRAP_ROOT  : Path to tree root
# YUNI_BOOTSTRAP_STAGE : Install root for nmosh

include(YuniRsrcSchemeRunners)

# set cache directory
set(ENV{NMOSH_CACHEDIR} "${YUNI_BOOTSTRAP_STAGE}/cache")

if(EXISTS "${YUNI_BOOTSTRAP_NMOSH}")
    # Nothing to do
else()
    message(FATAL_ERROR 
        "YUNI_BOOTSTRAP_NMOSH not found [${YUNI_BOOTSTRAP_NMOSH}]")
endif()


# Perform bootstrap

# Phase1(Generate yuni API stubs for all implementations)
run_nmosh_bootstrap(
    "${YUNI_BOOTSTRAP_NMOSH}" scripts/build-nmosh.sps 
    "${YUNI_BOOTSTRAP_ROOT}" rr)
if(rr)
    message(FATAL_ERROR "Failed phase1 (${rr})")
endif()

# Phase2(Generate NCCC API stubs)
run_nmosh(
    "${YUNI_BOOTSTRAP_NMOSH}" scripts/build-apistubs-nmosh.sps
    "${YUNI_BOOTSTRAP_ROOT}" rr)
if(rr)
    message(FATAL_ERROR "Failed phase2 (${rr})")
endif()

# Phase3(Same as phase1)
run_nmosh_bootstrap(
    "${YUNI_BOOTSTRAP_NMOSH}" scripts/build-nmosh.sps 
    "${YUNI_BOOTSTRAP_ROOT}" rr)
if(rr)
    message(FATAL_ERROR "Failed phase3 (${rr})")
endif()


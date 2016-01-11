# 
# Yuni CTest project
#
# INPUTs:
#
#  YUNI_TESTING_NMOSH_WIN64 : Path for nmosh(Win64)
#  YUNI_TESTING_ROOT : Path for yuni library root


project(yunitest)
cmake_minimum_required(VERSION 3.0)
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/../../cmake)

include(YuniRsrcSchemeRunners)

enable_testing()

if(EXISTS ${YUNI_TESTING_NMOSH_WIN64})
    set(WITH_NMOSH_WIN64 TRUE)
endif()

# Shortcuts

macro(test_nmosh0 nam platform nmosh script)
    add_nmosh_test(${nam} ${platform} ${nmosh} 
        ${YUNI_TESTING_ROOT}/${script}
        ${YUNI_TESTING_ROOT})
endmacro()

macro(test_nmosh_win64 nam script)
    test_nmosh0(${nam} WIN64 ${YUNI_TESTING_NMOSH_WIN64} ${script})
endmacro()

# Tests

if(WITH_NMOSH_WIN64)
    test_nmosh_win64(nmosh_win64_sanity _sanity.sps)
    test_nmosh_win64(nmosh_win64_ncccsanity _ncccsanity.sps)
endif()


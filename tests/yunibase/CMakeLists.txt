# testing with yunibase
#
# INPUTs:
#   YUNI_BASEDIR = root of yuni repository
#   YUNIBASE_YUNIFIED_PATH = yunified runner path

enable_testing()

macro(check_impl var nam)
    if(WIN32)
        set(_pth ${YUNIBASE_YUNIFIED_PATH}/${nam}.bat)
    else()
        set(_pth ${YUNIBASE_YUNIFIED_PATH}/${nam})
    endif()
    if(EXISTS ${_pth})
        set(YUNITEST_${var} ${_pth})
        message(STATUS "test: ${var} = ${_pth}")
    else()
        message(STATUS "test: ${var} not found (${_pth})")
    endif()

    # Boot
    if(WIN32)
        set(_pth ${YUNI_YUNIBOOT_PATH}/${nam}.bat)
    else()
        set(_pth ${YUNI_YUNIBOOT_PATH}/${nam})
    endif()
    if(EXISTS ${_pth})
        set(YUNIBOOTTEST_${var} ${_pth})
        message(STATUS "test(bootstrap): ${var} = ${_pth}")
    else()
        message(STATUS "test(bootstrap): ${var} not found (${_pth})")
    endif()
endmacro()

check_impl(CHIBI_SCHEME chibi-scheme)
check_impl(GOSH gosh)
check_impl(GUILE guile)
check_impl(RACKET racket)
check_impl(SAGITTARIUS sagittarius)
check_impl(CSI csi)
check_impl(VICARE vicare)
check_impl(NMOSH nmosh)
check_impl(KAWA kawa)
check_impl(LARCENY larceny)
check_impl(PETITE_CHEZ_SCHEME petite-chez-scheme)
check_impl(CHEZ_SCHEME chez-scheme)
check_impl(GSI gsi)
check_impl(RAPID_GAMBIT rapid-gambit)
check_impl(PICRIN picrin)
check_impl(MIT_SCHEME mit-scheme)
check_impl(IRON_SCHEME ironscheme)

set(impls
    CHIBI_SCHEME GOSH GUILE RACKET SAGITTARIUS 
    CSI 
    VICARE NMOSH KAWA
    PETITE_CHEZ_SCHEME CHEZ_SCHEME
    # LARCENY 
    RAPID_GAMBIT
    # PICRIN
    GSI MIT_SCHEME IRON_SCHEME)

set(ffi_impls # Exclude NMOSH KAWA
    CHIBI_SCHEME GOSH GUILE RACKET SAGITTARIUS VICARE
    # LARCENY 
    CHEZ_SCHEME CSI RAPID_GAMBIT
    PICRIN
    GSI)

# Issue #38
if(${CMAKE_SYSTEM_PROCESSOR} MATCHES "i?86$")
    if(NOT WIN32)
        # 32bit linux.
        list(REMOVE_ITEM ffi_impls SAGITTARIUS)
    endif()
endif()

function(test_negative_boot prefix)
    # Issue #58
    if(${prefix} STREQUAL IRON_SCHEME)
        return()
    endif()
    if(YUNIBOOTTEST_${prefix})
        add_test(NAME "SanityNegative-${prefix}"
            COMMAND ${CMAKE_COMMAND} -DPROG=${YUNIBOOTTEST_${prefix}}
            -DARG=${CMAKE_CURRENT_LIST_DIR}/_negative.sps
            -P ${CMAKE_CURRENT_LIST_DIR}/negative.cmake)
    endif()
endfunction()

function(test_with_boot prefix fil)
    if(YUNIBOOTTEST_${prefix})
        add_test(NAME "yuniboot-${prefix}-${fil}"
            COMMAND ${YUNIBOOTTEST_${prefix}} ${fil}
            WORKING_DIRECTORY ${YUNI_BASEDIR})
    endif()
endfunction()

function(test_with_impl prefix fil)
    if(YUNITEST_${prefix})
        add_test(NAME "${prefix}-${fil}"
            COMMAND ${YUNITEST_${prefix}} ${fil}
            WORKING_DIRECTORY ${YUNI_BASEDIR})
    endif()
endfunction()

function(test_with_all fil)
    # Library basic tests
    foreach(e ${impls})
        test_with_impl(${e} ${fil})
    endforeach()
endfunction()

function(test_with_allffi fil)
    foreach(e ${ffi_impls})
        test_with_impl(${e} ${fil})
    endforeach()
endfunction()

# Add Simple tests first.
foreach(e ${impls})
    test_negative_boot(${e})
endforeach()

# Sanity
test_with_all(_sanity.sps)
#test_with_allffi(_ncccsanity.sps)

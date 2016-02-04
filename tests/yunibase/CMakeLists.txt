# testing with yunibase
#
# INPUTs:
#   YUNI_BASEDIR = root of yuni repository
#   YUNIBASE_YUNIFIED_PATH = yunified runner path

enable_testing()

macro(check_impl var nam)
    set(_pth ${YUNIBASE_YUNIFIED_PATH}/${nam})
    if(EXISTS ${_pth})
        set(YUNITEST_${var} ${_pth})
        message(STATUS "test: ${var} = ${_pth}")
    else()
        message(STATUS "test: ${var} not found (${_pth})")
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

set(impls
    CHIBI_SCHEME GOSH GUILE RACKET SAGITTARIUS CSI VICARE NMOSH)
set(ffi_impls # Exclude CSI VICARE NMOSH
    CHIBI_SCHEME GOSH GUILE RACKET SAGITTARIUS)

function(test_with_impl prefix fil)
    if(YUNITEST_${prefix})
        add_test(NAME "${prefix}-${fil}"
            COMMAND ${YUNITEST_${prefix}} ${fil}
            WORKING_DIRECTORY ${YUNI_BASEDIR})
    endif()
endfunction()

function(test_with_all fil)
    foreach(e ${impls})
        test_with_impl(${e} ${fil})
    endforeach()
endfunction()

function(test_with_allffi fil)
    foreach(e ${ffi_impls})
        test_with_impl(${e} ${fil})
    endforeach()
endfunction()

# Sanity
test_with_all(_sanity.sps)
test_with_allffi(_ncccsanity.sps)

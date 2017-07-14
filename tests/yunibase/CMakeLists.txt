# testing with yunibase
#
# INPUTs:
#   YUNI_BASEDIR = root of yuni repository
#   YUNIBASE_YUNIFIED_PATH = yunified runner path

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
        # message(STATUS "test: ${var} not found (${_pth})")
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
    LARCENY 
    RAPID_GAMBIT
    # PICRIN
    GSI MIT_SCHEME IRON_SCHEME)

set(ffi_impls # Exclude NMOSH KAWA
    CHIBI_SCHEME GOSH GUILE RACKET SAGITTARIUS VICARE
    # LARCENY 
    CHEZ_SCHEME CSI RAPID_GAMBIT
    PICRIN
    GSI)

set(bootstrap_impls
    CHIBI_SCHEME
    GOSH
    SAGITTARIUS
    IRON_SCHEME
    RACKET)

# Issue #38
if(${CMAKE_SYSTEM_PROCESSOR} MATCHES "i?86$")
    if(NOT WIN32)
        # 32bit linux.
        list(REMOVE_ITEM ffi_impls SAGITTARIUS)
    endif()
endif()

function(test_with_cleandir prefix nam prog)
    if(YUNITEST_${prefix})
        add_test(NAME ${nam}
            COMMAND ${CMAKE_COMMAND} -DPROG=${YUNITEST_${prefix}}
            -DARG=${CMAKE_CURRENT_LIST_DIR}/${prog}
            -DDIRNAME=${CMAKE_CURRENT_BINARY_DIR}/testdir-${prefix}-${prog}
            -P ${CMAKE_CURRENT_LIST_DIR}/runwithcleandir.cmake)
    endif()
endfunction()

function(test_with_cleandir2 prefix nam prog) # For std tests
    if(YUNITEST_${prefix})
        add_test(NAME ${nam}
            COMMAND ${CMAKE_COMMAND} -DPROG=${YUNITEST_${prefix}}
            -DARG=${YUNI_BASEDIR}/tests/vm/${prog}
            -DDIRNAME=${CMAKE_CURRENT_BINARY_DIR}/testdir2-${prefix}-${prog}
            -P ${CMAKE_CURRENT_LIST_DIR}/runwithcleandir.cmake)
    endif()
endfunction()

function(test_with_cleandir2_yunivm prefix nam prog) # For yunivm tests
    if(YUNITEST_${prefix})
        add_test(NAME ${nam}
            COMMAND ${CMAKE_COMMAND} -DPROG=${YUNITEST_${prefix}}
            -DLAUNCH=${YUNI_BASEDIR}/tests/vm/_launcher.sps
            -DSCRIPT=${YUNI_BASEDIR}/tests/vm/${prog}
            -DDIRNAME=${CMAKE_CURRENT_BINARY_DIR}/testdir2yunivm-${prefix}-${prog}
            -P ${CMAKE_CURRENT_LIST_DIR}/runwithcleandir.cmake)
    endif()
endfunction()

function(test_negative prefix)
    if(YUNITEST_${prefix})
        add_test(NAME "SanityNegative-${prefix}"
            COMMAND ${CMAKE_COMMAND} -DPROG=${YUNITEST_${prefix}}
            -DARG=${CMAKE_CURRENT_LIST_DIR}/_negative.sps
            -P ${CMAKE_CURRENT_LIST_DIR}/negative.cmake)
        add_test(NAME "SanityNegative2-${prefix}"
            COMMAND ${CMAKE_COMMAND} -DPROG=${YUNITEST_${prefix}}
            -DARG=${CMAKE_CURRENT_LIST_DIR}/_negative2.txt
            -P ${CMAKE_CURRENT_LIST_DIR}/negative.cmake)
    endif()
endfunction()

function(test_with_impl prefix fil)
    if(YUNITEST_${prefix})
        add_test(NAME "${prefix}-${fil}"
            COMMAND ${YUNITEST_${prefix}} ${fil}
            WORKING_DIRECTORY ${YUNI_BASEDIR})
    endif()
endfunction()

function(test_yunivm_with_impl prefix fn)
    if(YUNITEST_${prefix})
        add_test(NAME "${prefix}-yunivm-${fn}"
            COMMAND ${YUNITEST_${prefix}} 
            _launcher.sps -PROG ${fn}
            WORKING_DIRECTORY ${YUNI_BASEDIR}/tests/vm)
    endif()
endfunction()

function(test_with_all_except fil)
    set(lis ${impls})
    list(REMOVE_ITEM lis ${ARGN})

    foreach(e ${lis})
        test_with_impl(${e} ${fil})
    endforeach()
endfunction()

function(test_with_allcleandir_except fil)
    set(lis ${impls})
    list(REMOVE_ITEM lis ${ARGN})

    foreach(e ${lis})
        test_with_cleandir2(${e} "${e}-${fil}" ${fil})
    endforeach()
endfunction()

function(test_yunivm_with_all_except fil)
    set(lis ${impls})
    list(REMOVE_ITEM lis ${ARGN})

    foreach(e ${lis})
        test_yunivm_with_impl(${e} ${fil})
    endforeach()
endfunction()

function(test_yunivm_with_allcleandir_except fil)
    set(lis ${impls})
    list(REMOVE_ITEM lis ${ARGN})

    foreach(e ${lis})
        test_with_cleandir2_yunivm(${e} "${e}-yunivm-${fil}" ${fil})
    endforeach()
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

function(test_yunistub_bootstrap fil)
    # FIXME: Copy'n'paste
    set(gen_yuniffi_stubs
            ${CMAKE_CURRENT_LIST_DIR}/../../yuniruntime/yuniffistub.sps)

    foreach(e ${bootstrap_impls})
        if(YUNITEST_${e})
            set(wrkdir ${CMAKE_CURRENT_BINARY_DIR}/yunistubtest/${e})
            file(MAKE_DIRECTORY ${wrkdir})
            add_test(NAME "yuniffi-stub-${e}"
                COMMAND ${YUNITEST_${e}}
                ${gen_yuniffi_stubs}
                -BOGUS
                -CDIR ${wrkdir}
                -SCMDIR ${wrkdir}
                -FILE ${fil}
                WORKING_DIRECTORY ${wrkdir})
        endif()
    endforeach()
endfunction()

# Add Simple tests first.
foreach(e ${impls})
    test_negative(${e})
endforeach()

# Sanity
test_with_all(_sanity.sps)
#test_with_allffi(_ncccsanity.sps)

# yuniVM
#test_with_all_except(tests/vm/vmtest.sps KAWA)
test_with_all_except(tests/vm/vmtest2.sps KAWA NMOSH)
test_with_all_except(tests/vm/vmtest4.sps 
    NMOSH IRON_SCHEME LARCENY)
test_with_all_except(tests/vm/vmtest5.sps 
    NMOSH)
test_with_allcleandir_except(vmtest6.sps 
    NMOSH RACKET LARCENY IRON_SCHEME KAWA)
if(NOT YUNI_TEST_SKIP_LONGRUN)
    test_with_all_except(tests/vm/vmtest3.sps KAWA GUILE NMOSH)
    test_yunivm_with_all_except(vmtest4.sps KAWA GUILE NMOSH)
    test_yunivm_with_all_except(vmtest5.sps KAWA GUILE NMOSH)
    test_yunivm_with_allcleandir_except(vmtest6.sps 
        LARCENY KAWA GUILE NMOSH)
endif()
test_with_all(tests/vm/heaptest.sps)

# yunistub bootstraps
test_yunistub_bootstrap(
    ${CMAKE_CURRENT_LIST_DIR}/../app/ffitrivial/stub/testing_trivial.scm)

# File-ops
foreach(e ${impls})
    test_with_cleandir(${e} "FileOps-${e}" _fileops.sps)
endforeach()

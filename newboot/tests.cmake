include(CMakeParseArguments)

set(tests ${YUNIROOT}/tests)

function(add_selfboot_test0 impl expect_error script)
    get_filename_component(nam ${script} NAME_WE)
    set(testname ${nam}-${impl})
    if(${expect_error} STREQUAL ON)
        set(arg_experror -DEXPECT_ERROR=ON)
    else()
        set(arg_experror)
    endif()
    add_test(NAME ${testname}
        COMMAND ${CMAKE_COMMAND} -DIMPL=${impl}
        ${arg_experror}
        -P ${CMAKE_CURRENT_BINARY_DIR}/run.cmake
        ${script}
        ${ARGN})
    if(willfail_${testname})
        set_tests_properties(${testname}
            PROPERTIES WILL_FAIL TRUE)
    endif()
endfunction()

function(add_selfboot_test impl script)
    add_selfboot_test0(${impl} OFF ${script} ${ARGN})
endfunction()

function(add_selfboot_test_negative impl script)
    add_selfboot_test0(${impl} ON ${script} ${ARGN})
endfunction()

function(add_selfboot_test_all script)
    foreach(impl ${impls})
        add_selfboot_test(${impl} ${script} ${ARGN})
    endforeach()
endfunction()

function(add_selfboot_test_negative_all script)
    foreach(impl ${impls})
        add_selfboot_test_negative(${impl} ${script} ${ARGN})
    endforeach()
endfunction()

macro(negative_tests script)
    set(_rest ${ARGN})
    add_selfboot_test_negative_all(${script})
    if(_rest)
        negative_tests(${ARGN})
    endif()
endmacro()

macro(tests script)
    set(_rest ${ARGN})
    add_selfboot_test_all(${script})
    if(_rest)
        tests(${ARGN})
    endif()
endmacro()

function(runtest script)
    cmake_parse_arguments(runtest
        ""
        ""
        "ARGS"
        ${ARGN})
    add_selfboot_test_all(${script} ${runtest_ARGS})
endfunction()

set(expected_failures
    core2-MIT_SCHEME
    inexact1-MIT_SCHEME
    fail2-RACKET
    fecore0-KAWA
    iter0-GUILE3
    )

foreach(e ${expected_failures})
    set(willfail_${e} ON)
endforeach()

tests(
    ${tests}/scheme/core0.sps
    ${tests}/scheme/core1.sps
    ${tests}/scheme/core2.sps
    ${tests}/scheme/iter0.sps
    ${tests}/scheme/strings0.sps
    ${tests}/scheme/vectors0.sps
    ${tests}/scheme/bytevectors0.sps
    ${tests}/scheme/inexact0.sps
    ${tests}/scheme/inexact1.sps
    ${tests}/scheme/qq0.sps
    ${tests}/scheme/stx0.sps
    ${tests}/lib/minitest0.sps
    ${tests}/lib/lighteval0.sps
    ${tests}/lib/hashtables0.sps
    ${tests}/lib/miniread0.sps
    ${tests}/app/basic/app.sps
    )

negative_tests(
    ${tests}/fail/fail0.sps
    ${tests}/fail/fail1.sps
    ${tests}/fail/fail2.sps
    ${tests}/fail/fail3.sps
    ${tests}/fail/fail4.sps
    ${tests}/fail/fail5.sps
    ${tests}/fail/fail6.sps
    ${tests}/fail/fail7.sps
    )

runtest(${tests}/newboot/testarg0.sps ARGS SPLITHERE)
runtest(${tests}/newboot/testarg1.sps ARGS SPLITHERE a 1 c)

# YUNIFE

runtest(${tests}/yunife/fecore0.sps ARGS
    ROOT ${YUNIROOT})

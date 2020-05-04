include(CMakeParseArguments)

set(tests ${YUNIROOT}/tests)

function(add_selfboot_test0 impl expect_error iotest script)
    get_filename_component(nam ${script} NAME_WE)
    set(testname ${nam}-${impl})
    if(${expect_error} STREQUAL ON)
        set(arg_experror -DEXPECT_ERROR=ON)
    else()
        set(arg_experror)
    endif()
    if(${iotest} STREQUAL ON)
        set(arg_io -DEMPTYDIR=ON -DEMPTYDIRBASE=${CMAKE_CURRENT_BINARY_DIR}/tmp)
    else()
        set(arg_io)
    endif()
    add_test(NAME ${testname}
        COMMAND ${CMAKE_COMMAND} -DIMPL=${impl}
        ${arg_io}
        ${arg_experror}
        -P ${CMAKE_CURRENT_BINARY_DIR}/run.cmake
        ${script}
        ${ARGN})
    if(willfail_${testname})
        set_tests_properties(${testname}
            PROPERTIES WILL_FAIL TRUE)
    endif()
endfunction()

function(add_selfboot_test_ident)
    foreach(impl ${impls})
        add_selfboot_test0(${impl} OFF OFF
            ${tests}/lib/ident0.sps
            -IMPLNAME
            ${YUNI_IDENT_${impl}})
    endforeach()
endfunction()

function(add_selfboot_test impl script)
    add_selfboot_test0(${impl} OFF OFF ${script} ${ARGN})
endfunction()

function(add_selfboot_test_io impl script)
    add_selfboot_test0(${impl} OFF ON ${script} ${ARGN})
endfunction()

function(add_selfboot_test_negative impl script)
    add_selfboot_test0(${impl} ON OFF ${script} ${ARGN})
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

function(add_selfboot_test_io_all script)
    foreach(impl ${impls})
        add_selfboot_test_io(${impl} ${script} ${ARGN})
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

macro(io_tests script)
    set(_rest ${ARGN})
    add_selfboot_test_io_all(${script})
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

include(${CMAKE_CURRENT_LIST_DIR}/fails.cmake)

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
    ${tests}/scheme/qq1.sps
    ${tests}/scheme/stx0.sps
    ${tests}/lib/minitest0.sps
    ${tests}/lib/lighteval0.sps
    ${tests}/lib/hashtables0.sps
    ${tests}/lib/miniread0.sps
    ${tests}/app/basic/app.sps
    ${tests}/sibr/sibr0010vector.sps
    ${tests}/sibr/sibr0010string.sps
    ${tests}/sibr/sibr0011.sps
    ${tests}/sibr/sibr0012gen.sps
    )

io_tests( # Run on temporary/empty dir
    ${tests}/scheme/io0.sps
    )

negative_tests(
    ${tests}/err/fail0.sps
    ${tests}/err/fail1.sps
    ${tests}/err/fail2.sps
    ${tests}/err/fail3.sps
    ${tests}/err/fail4.sps
    ${tests}/err/fail5.sps
    ${tests}/err/fail6.sps
    ${tests}/err/fail7.sps
    )

runtest(${tests}/newboot/testarg0.sps ARGS SPLITHERE)
runtest(${tests}/newboot/testarg1.sps ARGS SPLITHERE a 1 c)

add_selfboot_test_ident()

# YUNIFE

runtest(${tests}/yunife/fecore0.sps ARGS
    ROOT ${YUNIROOT})

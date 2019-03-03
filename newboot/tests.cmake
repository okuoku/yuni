set(tests ${YUNIROOT}/tests)

function(add_selfboot_test0 impl expect_error script)
    get_filename_component(nam ${script} NAME_WE)
    if(${expect_error} STREQUAL ON)
        set(arg_experror -DEXPECT_ERROR=ON)
    else()
        set(arg_experror)
    endif()
    add_test(NAME ${nam}-${impl} 
        COMMAND ${CMAKE_COMMAND} -DIMPL=${impl}
        ${arg_experror}
        -P ${CMAKE_CURRENT_BINARY_DIR}/run.cmake
        ${script})
endfunction()

function(add_selfboot_test impl script)
    add_selfboot_test0(${impl} OFF ${script})
endfunction()

function(add_selfboot_test_negative impl script)
    add_selfboot_test0(${impl} ON ${script})
endfunction()

function(add_selfboot_test_all script)
    foreach(impl ${impls})
        add_selfboot_test(${impl} ${script})
    endforeach()
endfunction()

function(add_selfboot_test_negative_all script)
    foreach(impl ${impls})
        add_selfboot_test_negative(${impl} ${script})
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
        negative_tests(${ARGN})
    endif()
endmacro()

tests(
    ${tests}/lib/minitest0.sps
    ${tests}/lib/lighteval0.sps
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


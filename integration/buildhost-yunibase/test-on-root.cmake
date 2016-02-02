# Test yuni tree on root

set(_yunibase /build)
set(_myroot /yuniroot)
set(_myproject /yuniroot/yuni/integration/buildhost-yunibase)
set(_buildroot /yuniroot/build)
set(_mypath ${CMAKE_CURRENT_LIST_DIR})

file(MAKE_DIRECTORY ${_myroot})
file(MAKE_DIRECTORY ${_buildroot})

get_filename_component(_mysrc ${_mypath}/../.. ABSOLUTE)

message(STATUS "Copying tree ${_mysrc} => ${_myroot}")

file(COPY ${_mysrc}
    DESTINATION ${_myroot}
    PATTERN ".git" EXCLUDE)

message(STATUS "Configure (${_myproject})...")

function(execute_step str)
    execute_process(COMMAND ${ARGN}
        RESULT_VARIABLE rr
        OUTPUT_VARIABLE out
        ERROR_VARIABLE err)
    if(rr)
        message(STATUS "Stdout:\n${out}\n\n")
        message(STATUS "Stderr:\n${err}\n\n")
        message(FATAL_ERROR "Fail: [${str}] (${rr})")
    endif()
endfunction()


execute_step("Configure"
    ${CMAKE_COMMAND} -DYUNI_WITH_YUNIBASE=${_yunibase} ${_myproject}
    WORKING_DIRECTORY ${_buildroot})

message(STATUS "Build...")

execute_step("Build"
    ${CMAKE_COMMAND} --build .
    WORKING_DIRECTORY ${_buildroot})

message(STATUS "Setup...")

execute_step("Setup"
    ${CMAKE_COMMAND} --build . --target install
    WORKING_DIRECTORY ${_buildroot})

message(STATUS "Test...")

execute_process(COMMAND
    ${CMAKE_CTEST_COMMAND} 
    --output-on-failure
    .
    RESULT_VARIABLE rr
    WORKING_DIRECTORY ${_buildroot})

if(rr)
    message(FATAL_ERROR "Test failure: ${rr}")
endif()

message(STATUS "Done.")


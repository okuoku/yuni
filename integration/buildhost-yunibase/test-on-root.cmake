# INPUTs:
#
#  VERBOSE: Verbose output
#  CLEAN: CLEAN build dir on success
#  SKIP_LONGRUN: YUNI_TEST_SKIP_LONGRUN
#
# Test yuni tree on root

set(_yunibase /opt/yunibase)
set(_myroot /yuniroot)
set(_myproject /yuniroot/yuni)
set(_buildroot /yuniroot/build)
set(_yunified /yunified)
set(_vanilla /vanilla)
set(_mypath ${CMAKE_CURRENT_LIST_DIR})

set(ENV{CTEST_OUTPUT_ON_FAILURE} 1)

get_filename_component(_mysrc ${_mypath}/../.. ABSOLUTE)

function(execute_step str)
    if(NOT VERBOSE)
        execute_process(COMMAND ${ARGN}
            RESULT_VARIABLE rr
            OUTPUT_VARIABLE out
            ERROR_VARIABLE err
            )
        if(rr)
            message(STATUS "Stdout:\n${out}\n\n")
            message(STATUS "Stderr:\n${err}\n\n")
            message(FATAL_ERROR "Fail: [${str}] (${rr})")
        endif()
    else()
        execute_process(COMMAND ${ARGN}
            RESULT_VARIABLE rr
            )
        if(rr)
            message(FATAL_ERROR "Fail: [${str}] (${rr})")
        endif()
    endif()
endfunction()

if(EXISTS ${_myroot})
    message(STATUS "Removing previous tree ${_myroot}")
    execute_step("Remove tree"
        ${CMAKE_COMMAND} -E remove_directory
        ${_myroot})
endif()

file(MAKE_DIRECTORY ${_myroot})
file(MAKE_DIRECTORY ${_buildroot})

message(STATUS "Copying tree ${_mysrc} => ${_myroot}")

file(COPY ${_mysrc}
    DESTINATION ${_myroot}
    PATTERN ".git" EXCLUDE)

message(STATUS "Configure (${_myproject})...")

set(_longrun)
if(SKIP_LONGRUN)
    set(_longrun "-DYUNI_TEST_SKIP_LONGRUN=ON")
endif()
set(_ffitest)
if(SKIP_FFITESTS)
    set(_ffitest "-DYUNI_TEST_SKIP_FFI=ON")
endif()

execute_step("Configure"
    ${CMAKE_COMMAND} 
    -DYUNI_WITH_YUNIBASE=${_yunibase} 
    -DYUNIBASE_YUNIFIED_PATH=${_yunified}
    -DYUNIBASE_VANILLA_PATH=${_vanilla}
    ${_longrun}
    ${_ffitest}
    ${_myproject}
    WORKING_DIRECTORY ${_buildroot})

message(STATUS "Build...")

if(EXISTS ${_buildroot}/Makefile)
    if(VERBOSE)
        set(para_count "")
    else()
        set(para_count "-j16")
    endif()
    execute_step("Build(Make)"
        make ${para_count}
        WORKING_DIRECTORY ${_buildroot})
else()
    execute_step("Build"
        ${CMAKE_COMMAND} --build .
        WORKING_DIRECTORY ${_buildroot})
endif()

message(STATUS "Setup...")

execute_step("Setup"
    ${CMAKE_COMMAND} --build . --target install
    WORKING_DIRECTORY ${_buildroot})

message(STATUS "Test...")

execute_process(COMMAND
    ${CMAKE_CTEST_COMMAND} 
    -j8
    --output-on-failure
    .
    RESULT_VARIABLE rr
    WORKING_DIRECTORY ${_buildroot})

if(rr)
    message(FATAL_ERROR "Test failure: ${rr}")
endif()

if(CLEAN)
    if(EXISTS ${_buildroot})
        message(STATUS "Removing builddir...")
        execute_process(COMMAND
            ${CMAKE_COMMAND} -E remove_directory ${_buildroot})
    endif()
endif()

message(STATUS "Done.")

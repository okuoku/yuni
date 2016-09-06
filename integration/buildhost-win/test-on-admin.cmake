# Test yuni tree bootstrap on Administrator account
#
# INPUTs:
#  BOOTSTRAP: Which scheme to use with bootstrap and build yuni
#

get_filename_component(_myroot ${CMAKE_CURRENT_LIST_DIR}/../.. ABSOLUTE)

set(workdir ${CMAKE_CURRENT_BINARY_DIR}/work)

message(STATUS "workdir = ${workdir}")

function(download_installer filename)
    set(base
        "http://storage.osdev.info/pub/proj/yuni/")
    file(DOWNLOAD
        ${base}/${filename}
        ${workdir}/${filename}
        TLS_VERIFY OFF)
endfunction()

function(do_build_and_test_yuni)
    set(ENV{PATH}
        "c:\\msys64\\mingw32\\bin\;$ENV{PATH}")
    # Configure yuni
    message(STATUS "Configure...")
    execute_process(
        COMMAND ${CMAKE_COMMAND}
        -G Ninja
        -DCMAKE_C_COMPILER=c:/msys64/mingw32/bin/gcc.exe
        -DCMAKE_CXX_COMPILER=c:/msys64/mingw32/bin/g++.exe
        ${_myroot}
        RESULT_VARIABLE rr
        WORKING_DIRECTORY ${workdir})

    if(rr)
        message(FATAL_ERROR "Failed to configure yuni: ${rr}")
    endif()

    # Build yuni
    message(STATUS "Build...")
    execute_process(
        COMMAND ${CMAKE_COMMAND}
        --build .
        RESULT_VARIABLE rr
        WORKING_DIRECTORY ${workdir})

    if(rr)
        message(FATAL_ERROR "Failed to build yuni: ${rr}")
    endif()

    # Test yuni
    message(STATUS "Test...")
    execute_process(
        COMMAND ${CMAKE_COMMAND}
        --build . --target test
        RESULT_VARIABLE rr
        WORKING_DIRECTORY ${workdir})

    if(rr)
        message(FATAL_ERROR "Failed to test yuni: ${rr}")
    endif()

endfunction()

message(STATUS "BOOTSTRAP = ${BOOTSTRAP}")

file(MAKE_DIRECTORY ${workdir})

if("${BOOTSTRAP}" STREQUAL gauche32)
    set(installer Gauche-mingw-0.9.4.msi)
    message(STATUS "Download...")
    download_installer(${installer})
    message(STATUS "Install...")
    # Install Gauche
    execute_process(
        COMMAND # start /wait
        msiexec /i ${installer} /quiet /qn /norestart
        RESULT_VARIABLE rr
        WORKING_DIRECTORY ${workdir})
    if(rr)
        message(FATAL_ERROR "Failed to install Gauche ${rr}")
    endif()

    do_build_and_test_yuni()
else()
    message(FATAL_ERROR "Unknown bootstrapper: ${BOOTSTRAP}")
endif()

message(STATUS "Done(${BOOTSTRAP})...")

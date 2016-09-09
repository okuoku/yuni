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

function(do_build_and_test_yuni bootstrapuse)
    set(ENV{PATH}
        "c:\\msys64\\mingw32\\bin\;$ENV{PATH}")
    # Configure yuni
    message(STATUS "Configure...")
    execute_process(
        COMMAND ${CMAKE_COMMAND}
        -G Ninja
        -DCMAKE_C_COMPILER=c:/msys64/mingw32/bin/gcc.exe
        -DCMAKE_CXX_COMPILER=c:/msys64/mingw32/bin/g++.exe
        -DYUNI_BOOTSTRAP_USE=${bootstrapuse}
        -DYUNI_IRON_SCHEME_ROOT=${workdir}/IronScheme
        ${_myroot}
        RESULT_VARIABLE rr
        WORKING_DIRECTORY ${workdir})

    if(rr)
        message(FATAL_ERROR "Failed to configure yuni(${bootstrapuse}): ${rr}")
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

function(install_gauche32)
    set(installer Gauche-mingw-0.9.4.msi)
    message(STATUS "Download gauche...")
    download_installer(${installer})
    message(STATUS "Install gauche...")
    # Install Gauche
    execute_process(
        COMMAND # start /wait
        msiexec /i ${installer} /quiet /qn /norestart
        RESULT_VARIABLE rr
        WORKING_DIRECTORY ${workdir})
    if(rr)
        message(FATAL_ERROR "Failed to install Gauche ${rr}")
    endif()
endfunction()

function(install_sagittarius installer)
    message(STATUS "Download Sagittarius(${installer})...")
    download_installer(${installer})
    message(STATUS "Install Sagittarius...")
    # Install Gauche
    execute_process(
        COMMAND # start /wait
        ${installer} /verysilent /norestart
        RESULT_VARIABLE rr
        WORKING_DIRECTORY ${workdir})
    if(rr)
        message(FATAL_ERROR "Failed to install Sagittarius ${rr}")
    endif()
endfunction()

function(install_ironscheme)
    set(archive "IronScheme-1.0.101-0fdbfcf.zip")
    message(STATUS "Download IronScheme...")
    download_installer(${archive})
    message(STATUS "Extract IronScheme...")
    # Extract
    execute_process(
        COMMAND
        ${CMAKE_COMMAND} -E tar xf
        ${archive}
        RESULT_VARIABLE rr
        WORKING_DIRECTORY ${workdir})
    if(rr)
        message(WARNING "Failed to extract IronScheme ${rr}")
    endif()
endfunction()

function(install32)
    install_gauche32()
    install_sagittarius(setup_sagittarius_0.7.7.exe) # 32bit
    install_ironscheme()
endfunction()

message(STATUS "BOOTSTRAP = ${BOOTSTRAP}")

file(MAKE_DIRECTORY ${workdir})

if(NOT BOOTSTRAP)
    set(BOOTSTRAP sagittarius32)
endif()

if("${BOOTSTRAP}" STREQUAL gauche32)
    install32()
    do_build_and_test_yuni(gauche)
elseif("${BOOTSTRAP}" STREQUAL sagittarius32)
    install32()
    do_build_and_test_yuni(sagittarius)
else()
    message(FATAL_ERROR "Unknown bootstrapper: ${BOOTSTRAP}")
endif()

message(STATUS "Done(${BOOTSTRAP})...")

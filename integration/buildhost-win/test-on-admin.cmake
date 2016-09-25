# Test yuni tree bootstrap on Administrator account
#
# INPUTs:
#  BOOTSTRAP: Which scheme to use with bootstrap and build yuni
#  SKIPINSTALL: Skip installation phase for test
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

function(do_build_and_test_yuni bitness bootstrapuse)
    if(${bitness} STREQUAL 32)
        set(mingw mingw32)
    else()
        set(mingw mingw64)
    endif()
    set(ENV{PATH}
        "c:\\msys64\\${mingw}\\bin\;$ENV{PATH}")
    # Configure yuni
    message(STATUS "Configure...")
    if(EXISTS ${workdir}/kawa.jar)
        set(kawa_arg -DYUNI_KAWA_JAR=${workdir}/kawa.jar)
    else()
        set(kawa_arg)
    endif()
    execute_process(
        COMMAND ${CMAKE_COMMAND}
        -G Ninja
        -DCMAKE_C_COMPILER=c:/msys64/${mingw}/bin/gcc.exe
        -DCMAKE_CXX_COMPILER=c:/msys64/${mingw}/bin/g++.exe
        -DYUNI_BOOTSTRAP_USE=${bootstrapuse}
        -DYUNI_IRON_SCHEME_ROOT=${workdir}/IronScheme
        ${kawa_arg}
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

    # Install yuni
    message(STATUS "Install...")
    execute_process(
        COMMAND ${CMAKE_COMMAND}
        --build .
        --target install
        WORKING_DIRECTORY ${workdir})

    if(rr)
        message(FATAL_ERROR "Failed to install yuni: ${rr}")
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

function(install_racket bitness installer)
    if(${bitness} STREQUAL 32)
        set(programfiles "c:/Program Files (x86)/Racket")
    else()
        set(programfiles "c:/Program Files/Racket")
    endif()
    message(STATUS "Download Racket(${installer})...")
    download_installer(${installer})
    message(STATUS "Install Racket...")
    # Install Gauche
    execute_process(
        COMMAND # start /wait
        ${installer} /S
        RESULT_VARIABLE rr
        WORKING_DIRECTORY ${workdir})
    if(rr)
        message(FATAL_ERROR "Failed to install Sagittarius ${rr}")
    endif()
    message(STATUS "Setting up R6RS/SRFI runtimes...")
    execute_process(
        COMMAND # start /wait
        "${programfiles}/raco.exe"
        pkg install --skip-installed --scope installation
        --auto srfi-lib r6rs-lib
        RESULT_VARIABLE rr
        WORKING_DIRECTORY ${workdir})
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

function(install_kawa)
    set(archive "kawa.jar")
    message(STATUS "Download Kawa...")
    download_installer(${archive})
endfunction()

function(install32)
    if(NOT SKIPINSTALL)
        install_gauche32()
        install_sagittarius(setup_sagittarius_0.7.8.exe) # 32bit
        install_ironscheme()
        install_racket(32 racket-minimal-6.6-i386-win32.exe)
    endif()
endfunction()

function(install64)
    if(NOT SKIPINSTALL)
        install_sagittarius(setup_sagittarius_0.7.8_x64.exe) # 64bit
        install_ironscheme()
        install_kawa()
        install_racket(64 racket-minimal-6.6-x86_64-win32.exe)
    endif()
endfunction()

message(STATUS "BOOTSTRAP = ${BOOTSTRAP}")

file(MAKE_DIRECTORY ${workdir})

if(NOT BOOTSTRAP)
    set(BOOTSTRAP sagittarius32)
endif()

if("${BOOTSTRAP}" STREQUAL gauche32)
    install32()
    do_build_and_test_yuni(32 gauche)
elseif("${BOOTSTRAP}" STREQUAL sagittarius32)
    install32()
    do_build_and_test_yuni(32 sagittarius)
elseif("${BOOTSTRAP}" STREQUAL sagittarius64)
    install64()
    do_build_and_test_yuni(64 sagittarius)
elseif("${BOOTSTRAP}" STREQUAL ironscheme32)
    install32()
    do_build_and_test_yuni(32 ironscheme)
elseif("${BOOTSTRAP}" STREQUAL ironscheme64)
    install64()
    do_build_and_test_yuni(64 ironscheme)
elseif("${BOOTSTRAP}" STREQUAL racket32)
    install32()
    do_build_and_test_yuni(32 racket)
elseif("${BOOTSTRAP}" STREQUAL racket64)
    install64()
    do_build_and_test_yuni(64 racket)
else()
    message(FATAL_ERROR "Unknown bootstrapper: ${BOOTSTRAP}")
endif()

message(STATUS "Done(${BOOTSTRAP})...")

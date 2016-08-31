##
## Yuni Main CMakeLists.txt
##
## MODEs:
##
##  Integration build:
##    YUNI_WITH_YUNIBASE=/path/to/yunibase/build
##    YUNI_SKIP_BOOTSTRAP=false (default)
##


set(CMAKE_LEGACY_CYGWIN_WIN32 0)
cmake_minimum_required(VERSION 2.8)
project(yuni)

enable_testing()

set(YUNI_WITH_YUNIBASE "" CACHE PATH "Yunibase directory.")

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/cmake)

set(YUNI_BASEDIR ${CMAKE_CURRENT_LIST_DIR})

# CMake modules
include(YuniDetectPlatform) 

yuni_detect_platform(_PLATFORM) # => WIN32 / WIN64 / CYG32 / CYG64 / UNKNOWN

message(STATUS "Platform: ${_PLATFORM}")

if(${_PLATFORM} STREQUAL WIN32)
    set(_POSTFIX "-win32")
elseif(${_PLATFORM} STREQUAL WIN64)
    set(_POSTFIX "-win64")
elseif(${_PLATFORM} STREQUAL CYG32)
    set(_POSTFIX "-cygwin32")
elseif(${_PLATFORM} STREQUAL CYG64)
    set(_POSTFIX "-cygwin64")
else()
    set(_POSTFIX "")
endif()

# YUNI_PLATFORM_LIBDIR: Path-to-nativecode
set(YUNI_PLATFORM_LIBDIR 
    ${YUNIBASE_YUNIFIED_PATH}/yunistub${_POSTFIX})

if(NOT YUNIBASE_YUNIFIED_PATH)
    set(YUNIBASE_YUNIFIED_PATH ${CMAKE_CURRENT_BINARY_DIR}/yunified)
endif()
if(NOT YUNIBASE_VANILLA_PATH)
    set(YUNIBASE_VANILLA_PATH ${CMAKE_CURRENT_BINARY_DIR}/vanilla)
endif()

# YUNIBASE: Use Yunibase's prebuilt implementation to build/test with
if(YUNI_WITH_YUNIBASE)

    # FIXME: Add current paths for linked-libraries
    set(CMAKE_LIBRARY_PATH
        ${YUNI_WITH_YUNIBASE}/current/chibi-scheme/lib
        ${CMAKE_LIBRARY_PATH})
    set(CMAKE_INCLUDE_PATH
        ${YUNI_WITH_YUNIBASE}/current/chibi-scheme/include
        ${CMAKE_INCLUDE_PATH})
    set(CMAKE_PROGRAM_PATH
        ${YUNI_WITH_YUNIBASE}/current/chibi-scheme/bin
        ${CMAKE_PROGRAM_PATH})

    # Add VANILLA path for find_program
    set(CMAKE_PROGRAM_PATH ${YUNIBASE_VANILLA_PATH} ${CMAKE_PROGRAM_PATH})

    # Emit runners from yunibase
    include(YuniBaseGenRunners)
    emit_yunibase_runners()
endif()

macro(yuni_install_native_targets)
    install(TARGETS ${ARGN}
        DESTINATION ${YUNI_PLATFORM_LIBDIR})
endmacro()
macro(yuni_install_native_files)
    install(FILES ${ARGN}
        DESTINATION ${YUNI_PLATFORM_LIBDIR})
endmacro()
macro(yuni_install_native_programs)
    install(PROGRAMS ${ARGN}
        DESTINATION ${YUNI_PLATFORM_LIBDIR})
endmacro()

# Setup scheme implementation paths for vanilla interpreters
include(YuniDetectScheme)

# Options

# Components (include testing)
add_subdirectory(apidata)
add_subdirectory(yunistub) # Currently this one includes test stubs
if(YUNI_WITH_YUNIBASE)
    add_subdirectory(tests/yunibase tests-yunibase)
endif()

# Status
if(YUNI_WITH_YUNIBASE)
    message(STATUS "YUNI_WITH_YUNIBASE = ${YUNI_WITH_YUNIBASE}")
endif()
message(STATUS "Gauche:           ${YUNI_GOSH}")
message(STATUS "  Gauche-package: ${YUNI_GAUCHE_PACKAGE}")
message(STATUS "chibi-scheme:     ${YUNI_CHIBI_SCHEME}")
message(STATUS "       chibi-ffi: ${YUNI_CHIBI_FFI}")
message(STATUS "             lib: ${YUNI_CHIBI_LIB}")
message(STATUS "Chicken:          ${YUNI_CHICKEN}")
message(STATUS "             csc: ${YUNI_CHICKEN_CSC}")
message(STATUS "             csi: ${YUNI_CHICKEN_CSI}")
message(STATUS "Gambit:           ${YUNI_GSC}")


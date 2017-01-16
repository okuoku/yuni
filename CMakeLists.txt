##
## Yuni Main CMakeLists.txt
##
## INPUT:
##    YUNI_BOOTSTRAP_USE=chibi-scheme | gauche | sagittarius
##    YUNI_IRON_SCHEME_ROOT
##
## MODEs:
##
##  Integration build:
##    YUNI_WITH_YUNIBASE=/path/to/yunibase/build
##    YUNI_SKIP_BOOTSTRAP=false (default)
##

cmake_minimum_required(VERSION 2.8.4)
project(yuni)

enable_testing()

set(YUNI_WITH_YUNIBASE "" CACHE PATH "Yunibase directory.")

list(APPEND CMAKE_MODULE_PATH 
    # Private modules
    ${CMAKE_CURRENT_LIST_DIR}/cmake
    # Public modules
    ${CMAKE_CURRENT_LIST_DIR}/cmake/public)

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

if(NOT YUNIBASE_YUNIFIED_PATH) # Legacy
    set(YUNIBASE_YUNIFIED_PATH ${CMAKE_CURRENT_BINARY_DIR}/yunified)
endif()

set(YUNISDK_ROOT ${YUNIBASE_YUNIFIED_PATH})

if(NOT YUNIBASE_VANILLA_PATH)
    set(YUNIBASE_VANILLA_PATH ${CMAKE_CURRENT_BINARY_DIR}/vanilla)
endif()

if(NOT YUNI_YUNIBOOT_PATH)
    set(YUNI_YUNIBOOT_PATH ${CMAKE_CURRENT_BINARY_DIR}/yuniboot)
endif()

# YUNI_BUILDCONFIG_LIBDIR: Path-to-yuniconfig libs
set(YUNI_BUILDCONFIG_LIBDIR
    ${CMAKE_CURRENT_BINARY_DIR}/buildconfig/lib)

# YUNI_PLATFORM_LIBDIR: Path-to-nativecode
set(YUNI_PLATFORM_LIBDIRNAME
    yunistub${_POSTFIX})
set(YUNI_PLATFORM_LIBDIR 
    ${YUNIBASE_YUNIFIED_PATH}/${YUNI_PLATFORM_LIBDIRNAME})

include(YuniBaseGenRunners)

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

    # Find kawa
    if(NOT YUNI_KAWA_JAR)
        if(EXISTS "${YUNI_WITH_YUNIBASE}/current/kawa/kawa.jar")
            set(YUNI_KAWA_JAR
                "${YUNI_WITH_YUNIBASE}/current/kawa/kawa.jar")
        endif()
    endif()

    # Emit runners from yunibase
    emit_yunibase_runners()
endif()


# Setup scheme implementation paths for vanilla interpreters
include(YuniDetectScheme)

# Bootstrap
# => yuni_bootstrap, YUNI_BOOTSTRAP_USE
# => yuni_package, yuni_package_build
add_subdirectory(bootstrap)

# FIXME: Guess these
set(YUNIBUILD_RUNTIME_ROOT ${YUNIBASE_YUNIFIED_PATH})
set(YUNIBUILD_BOOTSTRAP_USE ${YUNI_BOOTSTRAP_USE})
set(YUNIBUILD_RUNTIME_DEPENDS yuni_package_build yuni_bootstrap)

include(YuniDetectBootstrapScheme)

include(YuniBuild)
include(YuniRuntimeDeploy)

# Components
add_subdirectory(yunistub)
add_subdirectory(yuniruntime)
add_subdirectory(cmake/public cmake-public)

# Tests
enable_testing()
add_subdirectory(tests/yunibase tests-yunibase)
add_subdirectory(tests/app tests-app)

# Reset target

add_custom_target(reset
    COMMAND ${CMAKE_COMMAND} -E remove_directory 
    ${CMAKE_CURRENT_BINARY_DIR}/bootstrap
    COMMAND ${CMAKE_COMMAND} -E remove_directory 
    ${CMAKE_CURRENT_BINARY_DIR}/ffistub
    COMMAND ${CMAKE_COMMAND} -E remove_directory 
    ${CMAKE_CURRENT_BINARY_DIR}/package
    COMMAND ${CMAKE_COMMAND} -E remove_directory 
    ${YUNIBASE_YUNIFIED_PATH}
    COMMAND ${CMAKE_COMMAND} -E remove_directory 
    ${YUNIBASE_VANILLA_PATH}
    COMMAND ${CMAKE_COMMAND} -E remove_directory 
    ${YUNI_YUNIBOOT_PATH}
    COMMAND ${CMAKE_COMMAND} -E remove_directory
    ${YUNI_BUILDCONFIG_LIBDIR}
    COMMAND ${CMAKE_COMMAND} .
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

# Status

message(STATUS "")
message(STATUS "Configuration status:")
message(STATUS "")

message(STATUS "chibi-scheme:     ${YUNI_CHIBI_SCHEME}")
if(YUNI_CHIBI_SCHEME)
    message(STATUS "       chibi-ffi: ${YUNI_CHIBI_FFI}")
    message(STATUS "             lib: ${YUNI_CHIBI_SCHEME_LIB}")
endif()
message(STATUS "Gauche:           ${YUNI_GOSH}")
if(YUNI_GOSH)
    message(STATUS "  Gauche-package: ${YUNI_GAUCHE_PACKAGE}")
endif()
message(STATUS "Guile:            ${YUNI_GUILE}")
message(STATUS "Racket:           ${YUNI_RACKET}")
if(YUNI_RACKET)
    message(STATUS "            raco: ${YUNI_RACO}")
endif()
message(STATUS "Sagittarius:      ${YUNI_SAGITTARIUS}")
message(STATUS "Chicken:          ${YUNI_CHICKEN}")
if(YUNI_CHICKEN)
    message(STATUS "             csc: ${YUNI_CHICKEN_CSC}")
    message(STATUS "             csi: ${YUNI_CHICKEN_CSI}")
endif()
message(STATUS "Vicare:           ${YUNI_VICARE}")
message(STATUS "NMosh:            ${YUNI_NMOSH}")
message(STATUS "Larceny:          ${YUNI_LARCENY}")
message(STATUS "ChezScheme:       ${YUNI_CHEZ_SCHEME}") # Yuni has no petit
message(STATUS "Gambit:           ${YUNI_GSC}")
if(YUNI_GSC)
    message(STATUS "             gsi: ${YUNI_GSI}")
endif()
message(STATUS "Picrin:           ${YUNI_PICRIN}")
message(STATUS "MIT/GNU Scheme:   ${YUNI_MIT_SCHEME}")
message(STATUS "RapidGambit:      ${YUNI_RAPID_GAMBIT}")
message(STATUS "IronScheme:       ${YUNI_IRON_SCHEME}")
message(STATUS "Kawa:             ${YUNI_KAWA_JAR}")

if(YUNI_WITH_YUNIBASE)
    message(STATUS "YUNI_WITH_YUNIBASE = ${YUNI_WITH_YUNIBASE}")
endif()
if(YUNI_BOOTSTRAP_USE)
    message(STATUS "YUNI_BOOTSTRAP_USE = ${YUNI_BOOTSTRAP_USE}")
endif()
message(STATUS "")


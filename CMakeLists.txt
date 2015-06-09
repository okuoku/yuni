set(CMAKE_LEGACY_CYGWIN_WIN32 0)
cmake_minimum_required(VERSION 2.8)
project(yuni)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/cmake)

# CMake modules
include(YuniDetectPlatform) 
include(YuniDetectScheme)
include(YuniDetectScheme)

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

set(YUNI_PLATFORM_LIBDIR ${CMAKE_CURRENT_LIST_DIR}/lib-stub/yunistub${_POSTFIX})

macro(yuni_install_native_targets)
    install(TARGETS ${ARGN}
        DESTINATION ${YUNI_PLATFORM_LIBDIR})
endmacro()

# Options

# Components (include testing)

add_subdirectory(apidata)
add_subdirectory(yunistub)

# Status
message(STATUS "Gauche:       ${YUNI_GOSH}")
message(STATUS "chibi-scheme: ${YUNI_CHIBI_SCHEME}")
message(STATUS "   chibi-ffi: ${YUNI_CHIBI_FFI}")
message(STATUS "         lib: ${YUNI_CHIBI_LIB}")

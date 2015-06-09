if(__YUNI_DETECTSCHEME_INCLUDED)
    return()
endif()

set(__YUNI_DETECTSCHEME_INCLUDED 1)

message(STATUS "System: ${CMAKE_HOST_SYSTEM_NAME}")
message(STATUS "Processor: ${CMAKE_HOST_SYSTEM_PROCESSOR}")

# Gauche

if(${CMAKE_HOST_SYSTEM_NAME} STREQUAL CYGWIN)
    if(${CMAKE_HOST_SYSTEM_PROCESSOR} STREQUAL x86_64)
        # Cygwin64
    else()
        # Cygwin32
        find_program(YUNI_GOSH NAMES gosh)
    endif()
else()
    find_program(YUNI_GOSH NAMES gosh)
endif()


# chibi-scheme
find_program(YUNI_CHIBI_SCHEME NAMES chibi-scheme)
find_program(YUNI_CHIBI_FFI NAMES chibi-ffi)
find_library(YUNI_CHIBI_LIB NAMES chibi-scheme)
if(NOT YUNI_CHIBI_LIB)
    # Second chance for Cygwin/Windows
    find_program(YUNI_CHIBI_LIB NAMES libchibi-scheme.dll)
endif()


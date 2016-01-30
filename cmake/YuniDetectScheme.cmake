if(__YUNI_DETECTSCHEME_INCLUDED)
    return()
endif()

set(__YUNI_DETECTSCHEME_INCLUDED 1)

message(STATUS "System: ${CMAKE_HOST_SYSTEM_NAME}")
message(STATUS "Processor: ${CMAKE_HOST_SYSTEM_PROCESSOR}")

# Gauche
find_program(YUNI_GOSH NAMES gosh)
find_program(YUNI_GAUCHE_PACKAGE NAMES gauche-package)

# chibi-scheme
find_program(YUNI_CHIBI_SCHEME NAMES chibi-scheme)
find_program(YUNI_CHIBI_FFI NAMES chibi-ffi)
find_library(YUNI_CHIBI_LIB NAMES chibi-scheme)
if(NOT YUNI_CHIBI_LIB)
    # Second chance for Cygwin/Windows
    find_program(YUNI_CHIBI_LIB NAMES libchibi-scheme.dll)
endif()


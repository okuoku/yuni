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

# Chicken
find_program(YUNI_CHICKEN
    NAMES
    chicken)

if(YUNI_CHICKEN)
    find_program(YUNI_CHICKEN_CSC 
        NAMES
        chicken-csc
        csc)
    find_program(YUNI_CHICKEN_CSI 
        NAMES
        chicken-csi
        csi)
endif()

# Gambit
find_program(YUNI_GSC NAMES gsc
    HINTS
    /usr/local/Gambit/bin)

if(WIN32)
    # On Win32, gsc requires gcc on PATH to compile modules
    find_program(YUNI_GCC NAMES gcc)
endif()

# Racket
if(WIN32)
    # Prevent CYGWIN build from checking 
    set(racket_hint_win32 "$ENV{ProgramFiles}/Racket")
endif()

find_program(YUNI_RACKET NAMES racket
    HINTS
    "${racket_hint_win32}")

find_program(YUNI_RACO NAMES raco
    HINTS
    "${racket_hint_win32}")

if(__YUNI_DETECTSCHEME_INCLUDED)
    return()
endif()

set(__YUNI_DETECTSCHEME_INCLUDED 1)

message(STATUS "System: ${CMAKE_HOST_SYSTEM_NAME}")
message(STATUS "Processor: ${CMAKE_HOST_SYSTEM_PROCESSOR}")


set(detect_scheme_hint_paths)

if(NOT _PLATFORM) # root
    message(FATAL_ERROR "fixme")
endif()

# Since host CMake can be a 32bit version, we use our own platform detection
if(WIN32)
    # Detect preferred "Program Files" path
    if(${_PLATFORM} STREQUAL WIN64)
        set(YUNI_WIN32_PROGRAM_PATH # Points "c:/Program Files/ always"
            "$ENV{ProgramW6432}")
    else()
        # Pray we use Win32 CMake..
        # FIXME: Use detect ABI against CMAKE_PROGRAM here.
        set(YUNI_WIN32_PROGRAM_PATH
            "$ENV{ProgramFiles}")
    endif()

    # Append hint paths
    foreach(e "Racket" "Gauche/bin" "Sagittarius" "MIT-GNU Scheme/bin")
        list(APPEND detect_scheme_hint_paths
            "${YUNI_WIN32_PROGRAM_PATH}/${e}")
    endforeach()
endif()

macro(detect_scheme var)
    find_program(${var} ${ARGN} HINTS ${detect_scheme_hint_paths})
    if(${var})
        # Yunibase will always have matching ABI
        if(NOT YUNI_WITH_YUNIBASE)
            if(WIN32)
                yuni_get_exe_abi(__abi ${${var}})
                message(STATUS "${${var}} = ${__abi}")
                if(NOT ${__abi} STREQUAL ${_PLATFORM})
                    message(STATUS "Unmatched ABI for ${var}. Disable.")
                    set(${var} FALSE CACHE PATH "" FORCE)
                endif()
            endif()
        endif()
    endif()
endmacro()

function(detect_ironscheme var)
    # FIXME: Implement IronScheme on NuGet/choco
    # FIXME: Prefer .net 4
    set(out YUNI_IRON_SCHEME-NOTFOUND)
    if(YUNI_IRON_SCHEME_ROOT)
        if(${_PLATFORM} STREQUAL WIN64)
            set(ironscheme "IronScheme.Console-v4.exe")
        else()
            set(ironscheme "IronScheme.Console32-v4.exe")
        endif()
        set(theironscheme "${YUNI_IRON_SCHEME_ROOT}/${ironscheme}")
        if(EXISTS "${theironscheme}")
            set(out "${theironscheme}")
        endif()
    endif()
    set(${var} ${out} PARENT_SCOPE)
endfunction()

# On Win32, gsc requires gcc on PATH to compile modules
find_program(YUNI_GCC NAMES gcc)

# chibi-scheme
detect_scheme(YUNI_CHIBI_SCHEME NAMES chibi-scheme)
detect_scheme(YUNI_CHIBI_FFI NAMES chibi-ffi)
find_library(YUNI_CHIBI_LIB NAMES chibi-scheme)
if(NOT YUNI_CHIBI_LIB)
    # Second chance for Cygwin/Windows
    find_program(YUNI_CHIBI_LIB NAMES libchibi-scheme.dll)
endif()

# Gauche
detect_scheme(YUNI_GOSH NAMES gosh)
if(NOT WIN32)
    detect_scheme(YUNI_GAUCHE_PACKAGE NAMES gauche-package)
else(YUNI_GCC)
    # 0.9.4 gauche-package does not work yet.
    # detect_scheme(YUNI_GAUCHE_PACKAGE NAMES gauche-package)
endif()

# Guile
detect_scheme(YUNI_GUILE NAMES guile)

# Racket
detect_scheme(YUNI_RACKET NAMES racket)
detect_scheme(YUNI_RACO NAMES raco)

# Sagittarius
if(WIN32)
    set(win32_sagittarius_name sash)
else()
    set(win32_sagittarius_name)
endif()

detect_scheme(YUNI_SAGITTARIUS NAMES sagittarius ${win32_sagittarius_name})

# Chicken
detect_scheme(YUNI_CHICKEN
    NAMES
    chicken)

if(YUNI_CHICKEN)
    detect_scheme(YUNI_CHICKEN_CSC 
        NAMES
        chicken-csc
        csc)
    detect_scheme(YUNI_CHICKEN_CSI 
        NAMES
        chicken-csi
        csi)
endif()

# Vicare
detect_scheme(YUNI_VICARE NAMES vicare)

# nmosh
detect_scheme(YUNI_NMOSH NAMES nmosh)

# Kawa is only supported on yunibase

# Larceny
if(WIN32)
    if(YUNI_LARCENY_ROOT)
        set(yuni_larceny_bin ${YUNI_LARCENY_ROOT}/larceny.bin.exe)
        if(EXISTS ${YUNI_LARCENY_ROOT}/larceny.bin.exe)
            set(YUNI_LARCENY ${yuni_larceny_bin})
        endif()
    endif()
else()
    detect_scheme(YUNI_LARCENY NAMES larceny.bin)
endif()

# Chez scheme
# FIXME: May conflict with mit-scheme
detect_scheme(YUNI_CHEZ_SCHEME NAMES chez-scheme scheme)
detect_scheme(YUNI_CHEZ_PETITE NAMES petite-chez-scheme petite)

# Gambit
detect_scheme(YUNI_GSC NAMES gsc
    HINTS
    /usr/local/Gambit/bin)
detect_scheme(YUNI_GSI NAMES gsi
    HINTS
    /usr/local/Gambit/bin)

# Picrin
detect_scheme(YUNI_PICRIN NAMES picrin)

# MIT-SCHEME
detect_scheme(YUNI_MIT_SCHEME NAMES mit-scheme)

# Rapid-gambit
detect_scheme(YUNI_RAPID_GAMBIT NAMES rapid-gambit)

# IronScheme
detect_ironscheme(YUNI_IRON_SCHEME)

# Kawa
if(YUNI_KAWA_JAR)
    find_package(Java)
    if(NOT Java_JAVA_EXECUTABLE)
        set(YUNI_KAWA_JAR FALSE)
    endif()
endif()

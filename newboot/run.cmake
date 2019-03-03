# Calc Command line argument
set(args)
set(cur 0)
while(NOT ${cur} EQUAL ${CMAKE_ARGC})
    set(arg "${CMAKE_ARGV${cur}}")
    if(${arg} STREQUAL "-P")
        set(args ${arg})
    elseif(args)
        list(APPEND args ${arg})
    endif()
    math(EXPR cur "${cur}+1")
endwhile()
# Drop 2 args for "-P <SCRIPT>"
list(REMOVE_AT args 0)
list(REMOVE_AT args 0)

if(NOT IMPL)
    message(FATAL_ERROR "Huh?")
endif()

if(NOT YUNI_${IMPL})
    message(FATAL_ERROR "Impl ${IMPL} did not found")
endif()

set(selfboot_CHIBI_SCHEME "chibi-scheme/selfboot-entry.scm")
set(selfboot_GOSH "gauche/selfboot-entry.scm")
set(selfboot_SAGITTARIUS "sagittarius/selfboot-entry.sps")
set(selfboot_CHEZ_SCHEME "chez/selfboot-entry.sps")
set(selfboot_S7 "s7/selfboot-entry.scm")
set(selfboot_RACKET "racket/selfboot-entry.rkt")
set(selfboot_CHICKEN_CSI "chicken/selfboot-entry.scm")
set(selfboot_GSI "gambit/selfboot-entry.scm")
set(selfboot_GUILE "guile/selfboot-entry.sps")
set(selfboot_BIWASYUNI "biwascheme/selfboot-entry.scm")

if(NOT selfboot_${IMPL})
    message(FATAL_ERROR "Impl ${IMPL} is not configured")
endif()

if(${IMPL} STREQUAL CHEZ_SCHEME)
    set(arg_prog "--program")
elseif(${IMPL} STREQUAL CHICKEN_CSI)
    set(arg_prog "-script")
else()
    set(arg_prog)
endif()

execute_process(
    COMMAND 
    ${YUNI_${IMPL}} 
    ${arg_prog}
    ${YUNIROOT}/lib-runtime/selfboot/${selfboot_${IMPL}}
    -LIBPATH .
    ${args}
    RESULT_VARIABLE rr
    )

if(rr)
    message(FATAL_ERROR "Err: ${rr}")
endif()

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

set(arg_prog ${arg_prog_${IMPL}})

list(GET args 0 script)
get_filename_component(appdir ${script} PATH)
if(NOT appdir)
    set(appdir .)
endif()

if(NOT selfboot_${IMPL})
    message(FATAL_ERROR "Impl ${IMPL} is not configured")
endif()
execute_process(
    COMMAND 
    ${YUNI_${IMPL}} 
    ${arg_prog}
    ${YUNIROOT}/lib-runtime/selfboot/${selfboot_${IMPL}}
    -LIBPATH ${appdir}
    ${args}
    RESULT_VARIABLE rr
    )

if(NOT EXPECT_ERROR)
    if(rr)
        message(FATAL_ERROR "Err: ${rr}")
    endif()
else()
    if(NOT rr)
        message(FATAL_ERROR "Expected error: ${rr}")
    endif()
endif()

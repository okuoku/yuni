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
set(arg_ci ${arg_ci_${IMPL}})
set(arg_post ${arg_post_${IMPL}})

set(arg_runner ${YUNIROOT}/lib-runtime/selfboot/${selfboot_${IMPL}})
if(arg_WAR_LAST_RUNNER_${IMPL})
    set(arg_runner2 ${arg_runner})
else()
    set(arg_runner2)
endif()

list(GET args 0 script)
get_filename_component(appdir ${script} PATH)
if(NOT appdir)
    set(appdir .)
endif()

if(EMPTYDIR)
    if(EMPTYDIRBASE)
        set(workdirbase ${EMPTYDIRBASE})
    else()
        set(workdirbase ${CMAKE_CURRENT_BINARY_DIR})
    endif()
    # Calc. randam pathname
    string(RANDOM rnd)
    set(workdir "${workdirbase}/${rnd}")
    set(arg_workdir WORKING_DIRECTORY "${workdir}")
else()
    set(workdir)
    set(arg_workdir)
endif()

if(workdir)
    file(MAKE_DIRECTORY ${workdir})
endif()

set(modpaths)
if(YUNI_INPLACE_NCCC_MODULES_PREFIX)
    list(APPEND modpaths -MODPATH ${YUNI_INPLACE_NCCC_MODULES_PREFIX})
endif()

if(NOT selfboot_${IMPL})
    message(FATAL_ERROR "Impl ${IMPL} is not configured")
endif()
execute_process(
    COMMAND 
    ${YUNI_${IMPL}} 
    ${arg_ci}
    ${arg_prog}
    ${arg_runner}
    ${arg_post}
    -LIBPATH ${appdir}
    ${modpaths}
    ${args}
    ${arg_runner2}
    RESULT_VARIABLE rr
    ${arg_workdir}
    )

message(STATUS "Result: ${rr}, Expect: ${EXPECT_ERROR}")

if(workdir)
    file(REMOVE_RECURSE "${workdir}")
endif()

if(UNIX)
    # Restore TTY
    execute_process(
        COMMAND stty sane)
endif()

if(NOT EXPECT_ERROR)
    if(rr)
        if(WILL_FAIL)
            message(FATAL_ERROR "FAIL: ${rr}")
        else()
            message(FATAL_ERROR "FAIL: ${rr} (treated as success)")
        endif()
    else()
        message(STATUS "Success.")
    endif()
else()
    if(NOT rr)
        message(FATAL_ERROR "FAIL: ${rr} (error expected)")
    else()
        message(STATUS "Success.")
    endif()
endif()

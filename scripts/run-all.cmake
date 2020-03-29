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

foreach(impl ${YUNIBOOT_IMPLS})
    execute_process(
        COMMAND ${CMAKE_COMMAND}
        -DIMPL=${impl}
        -P ${BOOTDIR}/run.cmake
        ${args}
        OUTPUT_VARIABLE oo_${impl}
        ERROR_VARIABLE ee_${impl}
        RESULT_VARIABLE rr_${impl})
    if(rr_${impl})
        message(STATUS "ERR: ${impl}")
    else()
        message(STATUS "OK : ${impl}")
    endif()
endforeach()

foreach(impl ${YUNIBOOT_IMPLS})
    if(rr_${impl})
        message(STATUS "ERR: ${impl} (${YUNI_${impl}})")
        message(STATUS "Out:\n${oo_${impl}}")
        message(STATUS "Err:\n${ee_${impl}}")
    else()
        message(STATUS "OK: ${impl}")
        message(STATUS "Out:\n${oo_${impl}}")
    endif()
endforeach()

message(STATUS)
message(STATUS "Summary: ")
message(STATUS)

foreach(impl ${YUNIBOOT_IMPLS})
    if(rr_${impl})
        message(STATUS "    ERR: ${impl}")
    else()
        message(STATUS "    OK : ${impl}")
    endif()
endforeach()

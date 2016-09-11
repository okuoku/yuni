#
# negative test stub
#
# INPUTs:
#  
#   PROG: Full path to program
#   ARG: Argument (single)

if(NOT PROG)
    message(FATAL_ERROR "Oh.")
endif()

execute_process(
    COMMAND ${PROG} ${ARG}
    RESULT_VARIABLE rr)

if(rr)
    message(STATUS "OK: ${rr}")
else()
    message(FATAL_ERROR "Unexpected: ${rr}")
endif()


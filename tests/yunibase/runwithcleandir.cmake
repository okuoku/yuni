#
# Run test with clean directory
#
# INPUTs:
#  
#   PROG: Full path to program
#   DIRNAME: Directory name
#   ARG: Argument (single)

if(NOT PROG)
    message(FATAL_ERROR "Oh.")
endif()

if(NOT DIRNAME)
    message(FATAL_ERROR "Oh.")
endif()

if(EXISTS ${DIRNAME})
    if(NOT IS_DIRECTORY ${DIRNAME})
        message(FATAL_ERROR "Why?")
    endif()
endif()

file(REMOVE_RECURSE ${DIRNAME})

file(MAKE_DIRECTORY ${DIRNAME})

execute_process(
    COMMAND ${PROG} ${ARG}
    RESULT_VARIABLE rr
    WORKING_DIRECTORY ${DIRNAME})

if(rr)
    message(FATAL_ERROR "Unexpected: ${rr}")
endif()


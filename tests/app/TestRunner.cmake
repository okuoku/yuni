# YuniApp test runner
#
# INPUT:
#  GEN: gen
#  RUN: run
#  APPDIR: appdir
#  WORKDIR: workdir
#  SH: /bin/sh if required

execute_process(
    COMMAND ${GEN} ${APPDIR}
    WORKING_DIRECTORY ${WORKDIR}
    RESULT_VARIABLE rr)

if(rr)
    message(FATAL_ERROR "Failed to generate ${APPDIR} (${GEN})")
endif()

execute_process(
    COMMAND ${SH} ${RUN}
    WORKING_DIRECTORY ${WORKDIR}
    RESULT_VARIABLE rr)

if(rr)
    message(FATAL_ERROR "Failed to run ${APPDIR} (${RUN})")
endif()


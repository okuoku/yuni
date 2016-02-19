# Test yuni tree on yunibuild Docker image
#
# INPUTs:
#   IMAGE = image name for doker (defaults to "okuoku/yunibase:testing")

get_filename_component(_myroot ${CMAKE_CURRENT_LIST_DIR}/../.. ABSOLUTE)

if(NOT IMAGE)
    set(IMAGE "okuoku/yunibase:testing")
endif()

macro(run_docker_image img rr)
    execute_process(COMMAND
        docker run -it --rm -v ${_myroot}:/yuni:Z ${img} cmake -P
        /yuni/integration/buildhost-yunibase/test-on-root.cmake
        RESULT_VARIABLE ${rr})
endmacro()

run_docker_image("${IMAGE}" rr1)

if(rr1)
    message(FATAL_ERROR "Fail on ${IMAGE}")
endif()


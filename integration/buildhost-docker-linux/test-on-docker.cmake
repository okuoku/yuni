# Test yuni tree on yunibuild Docker image

get_filename_component(_myroot ${CMAKE_CURRENT_LIST_DIR}/../.. ABSOLUTE)

macro(run_docker_image img rr)
    execute_process(COMMAND
        docker run -it --rm -v ${_myroot}:/yuni ${img} cmake -P
        /yuni/integration/buildhost-yunibase/test-on-root.cmake
        RESULT_VARIABLE ${rr})
endmacro()

run_docker_image("okuoku/yunibase:testing" rr)
run_docker_image("okuoku/yunibase:testing-fedora" rr)

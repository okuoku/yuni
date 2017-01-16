include(CMakeParseArguments)

function(yuni_runtime_deploy)
    set(_opts)
    set(_one)
    set(_multi TARGETS FILES)
    cmake_parse_arguments(ARG "${_opts}" "${_one}" "${_multi}" ${ARGN})

    if(ARG_TARGETS)
        install(TARGETS ${ARG_TARGETS}
            DESTINATION ${YUNI_PLATFORM_LIBDIR})
    endif()

    if(ARG_FILES)
        install(FILES ${ARG_FILES}
            DESTINATION ${YUNI_PLATFORM_LIBDIR})
    endif()

endfunction()

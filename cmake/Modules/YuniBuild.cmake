# Public CMake function/macros for Yuni apps
#
# INPUTs:
#   YUNIBUILD_RUNTIME_ROOT: Full path to yunified dir
#   YUNIBUILD_BOOTSTRAP_USE: Scheme implementation type to use
#   YUNIBUILD_BOOTSTRAP_COMMAND: Yuni script loader executable

include(CMakeParseArguments)

function(yunibuild_add_custom_command)
    set(acc)
    set(depend)
    set(mode NONE)
    foreach(e ${ARGN})
        if(${mode} STREQUAL SCRIPT)
            # Generate script arguments
            list(APPEND acc COMMAND 
                ${YUNIBUILD_BOOTSTRAP_COMMAND}
                "${e}")
            set(mode NONE)
        else() # NONE
            if("${e}" STREQUAL SCRIPT)
                set(mode SCRIPT)
            elseif("${e}" STREQUAL DEPENDS)
                set(depend TRUE)
                list(APPEND acc DEPENDS 
                    ${YUNIBUILD_RUNTIME_DEPENDS})
            else()
                list(APPEND acc "${e}")
            endif()
        endif()
    endforeach()
    if(NOT depend)
        list(APPEND DEPENDS ${YUNIBUILD_RUNTIME_DEPENDS})
    endif()
    add_custom_command(${acc})
endfunction()


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

function(yunibuild_add_yunistub tgt stubir scmoutdir coutdir)
    get_filename_component(stubirname ${stubir} NAME_WE)
    set(outc)
    foreach(e ${ARGN})
        list(APPEND outc ${coutdir}/${e})
    endforeach()
    set(outscm)
    foreach(e constants libstate)
        list(APPEND outscm ${scmoutdir}/${stubirname}-${e}.sls)
    endforeach()
    yunibuild_add_custom_command(OUTPUT ${outc} ${outscm}
        SCRIPT    
        ${YUNIBUILD_RUNTIME_ROOT}/loader/yuniffistub.sps
        -BOGUS # FIXME: IronScheme workaround
        -CDIR ${coutdir}
        -SCMDIR ${scmoutdir}
        -FILE ${stubir}
        DEPENDS 
        ${YUNIBUILD_RUNTIME_ROOT}/loader/yuniffistub.sps
        COMMENT "Processing StubIR0(${tgt})...")
    add_custom_target(${tgt} DEPENDS ${outc} ${outscm})
endfunction()


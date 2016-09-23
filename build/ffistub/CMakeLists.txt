set(top ${CMAKE_CURRENT_LIST_DIR}/../..)

# FIXME: Merge with package phase
if(${YUNI_BOOTSTRAP_USE} STREQUAL gauche)
    set(bootstrap_name gosh)
else()
    set(bootstrap_name ${YUNI_BOOTSTRAP_USE})
endif()

if(WIN32)
    set(bootstrap ${YUNIBASE_YUNIFIED_PATH}/${bootstrap_name}.bat)
else()
    set(bootstrap ${YUNIBASE_YUNIFIED_PATH}/${bootstrap_name})
endif()

set(gen_yuniffi_stubs
    ${CMAKE_CURRENT_LIST_DIR}/gen-yuniffi-stubs.sps)

function(add_yuniffi_stub tgt stubir scmdir cdir) # ARGN = csrcnames
    set(outc)
    foreach(e ${ARGN})
        list(APPEND outc ${cdir}/${e})
    endforeach()
    add_custom_command(OUTPUT ${outc}
        COMMAND
        ${bootstrap}
        ${gen_yuniffi_stubs}
        -BOGUS # FIXME: IronScheme workaround
        -CDIR ${cdir}
        -SCMDIR ${scmdir}
        -FILE ${stubir}
        DEPENDS ${gen_yuniffi_stubs}
        COMMENT "Processing StubIR0(${tgt})...")
    add_custom_target(${tgt} DEPENDS ${outc})
    add_dependencies(${tgt} yuni_package_build yuni_bootstrap)
endfunction()

add_subdirectory(${top}/apidata apidata)

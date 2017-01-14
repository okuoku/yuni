set(top ${CMAKE_CURRENT_LIST_DIR}/../..)
set(stubroot ${CMAKE_CURRENT_BINARY_DIR})

function(add_yuniffi_stub tgt stubir scmdir cdir) # ARGN = csrcnames
    get_filename_component(stubirname ${stubir} NAME_WE)
    set(outc)
    foreach(e ${ARGN})
        list(APPEND outc ${cdir}/${e})
    endforeach()
    set(outscm)
    foreach(e constants libstate)
        list(APPEND outscm ${scmdir}/${stubirname}-${e}.sls)
    endforeach()
    yunibuild_add_custom_command(OUTPUT ${outc} ${outscm}
        SCRIPT
        ${top}/yuniruntime/yuniffistub.sps
        -BOGUS # FIXME: IronScheme workaround
        -CDIR ${cdir}
        -SCMDIR ${scmdir}
        -FILE ${stubir}
        DEPENDS ${gen_yuniffi_stubs}
        yuni_package_build yuni_bootstrap
        COMMENT "Processing StubIR0(${tgt})...")
    add_custom_target(${tgt} DEPENDS ${outc} ${outscm})
endfunction()

function(gen_yuniffi_stub tgt stubir)
    file(MAKE_DIRECTORY
        ${stubroot}/yuniffi/yunistub)
    add_yuniffi_stub(${tgt} ${stubir} 
        ${stubroot}/yuniffi/yunistub
        ${CMAKE_CURRENT_BINARY_DIR}
        ${ARGN})
endfunction()

add_subdirectory(${top}/apidata apidata)

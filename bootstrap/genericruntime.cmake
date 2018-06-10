#
# Generic runtime 
#
# INPUTs:
#   yuniroot: Root 
#

## s7
set(generic_runtime_impl_s7_pre
    lib-runtime/s7/prelib.scm
    )

set(generic_runtime_impl_s7_runtime
    lib-runtime/generic/std/vector-map.scm
    )

## Biwascheme
set(generic_runtime_impl_biwascheme_pre
    lib-runtime/biwascheme/prelib.scm
    )
set(generic_runtime_impl_biwascheme_runtime
    lib-runtime/generic/std/vector-map.scm
    lib-runtime/generic/std/make-list.scm
    lib-runtime/generic/std/list-set_x.scm
    lib-runtime/generic/std/modulo.scm
    lib-runtime/generic/std/floor-remainder.scm
    lib-runtime/generic/std/floor-quotient.scm
    lib-runtime/generic/std/floor_div.scm
    lib-runtime/generic/std/quotient.scm
    lib-runtime/generic/std/remainder.scm
    lib-runtime/generic/std/truncate-remainder.scm
    lib-runtime/generic/std/truncate-quotient.scm
    lib-runtime/generic/std/truncate_div.scm
    )

## Generic
set(generic_runtime_pre
    lib-runtime/generic/synrules.scm
    external/yuni-synrules.scm
    )

set(generic_runtime_post
    lib-runtime/generic/verboselib.scm
    lib-runtime/generic/libmgr-file.scm
    lib-runtime/generic/libmgr-core.scm
    lib-runtime/generic/libmgr-macro.scm
    lib-runtime/generic/yuniloader-generic.scm
    )

macro(gen_runtime_filelist out impl)
    set(${out})
    foreach(e 
            ${generic_runtime_impl_${impl}_pre}
            ${generic_runtime_pre}
            ${generic_runtime_impl_${impl}_runtime}
            ${generic_runtime_post}
            )
        list(APPEND ${out} ${yuniroot}/${e})
    endforeach()
endmacro()

function(emit_generic_runtime impl pth)
    gen_runtime_filelist(files ${impl})
    get_filename_component(dir ${pth} PATH)
    file(MAKE_DIRECTORY ${dir})
    foreach(e ${files})
        file(READ ${e} file)
        set(content "${content}${file}\n")
    endforeach()
    file(WRITE ${pth} "${content}")
endfunction()

function(add_gen_generic_runtime tgt impl pth)
    set(me ${CMAKE_CURRENT_LIST_DIR}/genericruntime.cmake)
    gen_runtime_filelist(files ${impl})
    add_custom_command(OUTPUT ${pth}
        DEPENDS ${files} ${me}
        COMMAND ${CMAKE_COMMAND} -DDO_EMIT=ON -DIMPL=${impl} -DPATH=${pth}
        -DYUNIROOT=${yuniroot} -P ${me})
    add_custom_target(${tgt} DEPENDS ${pth})
endfunction()

if(DO_EMIT)
    if(NOT IMPL)
        message(FATAL_ERROR "Huh?")
    endif()
    if(NOT PATH)
        message(FATAL_ERROR "Huh?")
    endif()
    if(NOT YUNIROOT)
        message(FATAL_ERROR "Huh?")
    endif()
    set(yuniroot ${YUNIROOT})
    emit_generic_runtime(${IMPL} ${PATH})
endif()



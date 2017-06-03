# Always perform copy on MSBuild...
# We have no way to avoid BYPRODUCTS here.

set(runtimefiles)

function(installdep f)
    set(out ${YUNIBASE_YUNIFIED_PATH}/loader/${f})
    add_custom_command(OUTPUT ${out}
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${CMAKE_CURRENT_LIST_DIR}/${f}
        ${out}
        COMMENT "Copy ${f} to runtime path"
        DEPENDS ${f})
    list(APPEND runtimefiles ${out})
    set(runtimefiles ${runtimefiles} PARENT_SCOPE)
endfunction()

set(deps
    generate.sps
    yuniffistub.sps)

foreach(e ${deps})
    installdep(${e})
endforeach()

add_custom_target(yuniruntime-copy
    COMMENT "Copying runtime files (yuniruntime)"
    DEPENDS ${runtimefiles})

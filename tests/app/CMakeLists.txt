# Yuniapp testing


set(impls)
macro(set_impl nam sym gen)
    list(APPEND impls ${nam})
    set(IMPL_${nam}_gen ${gen})
    set(IMPL_${nam}_sym ${sym})
endmacro()

yuni_detect_platform(apptest_platform)

set_impl(RACKET       racket       racket)
set_impl(GUILE        guile        guile)
set_impl(CHIBI_SCHEME chibi-scheme chibi-scheme)
set_impl(GAUCHE       gauche       gosh)
set_impl(SAGITTARIUS  sagittarius  sagittarius)
set_impl(CHEZ_SCHEME  chez         chez-scheme)
set_impl(VICARE       vicare       vicare)

set(ffi_impls # Impls which capable to yuniffi
    RACKET GUILE 
    # CHIBI_SCHEME 
    GAUCHE SAGITTARIUS
    CHEZ_SCHEME VICARE)

function(add_apptest0 dir is_ffi)
    set(dirname ${CMAKE_CURRENT_BINARY_DIR}/${dir})
    set(appdir ${CMAKE_CURRENT_LIST_DIR}/${dir})
    if(${apptest_platform} STREQUAL WIN32)
        set(SH)
        set(scriptext ".bat")
        set(scriptextgen ".bat")
    elseif(${apptest_platform} STREQUAL WIN64)
        set(SH)
        set(scriptext ".bat")
        set(scriptextgen ".bat")
    else()
        set(SH -DSH=/bin/sh)
        set(scriptext ".sh")
        set(scriptextgen)
    endif()
    if(is_ffi)
        # Build Yuniffi modules
        add_subdirectory(${dir})
    else()
        file(REMOVE_RECURSE ${dirname})
        file(MAKE_DIRECTORY ${dirname})
    endif()
    if(is_ffi)
        set(cur_impls ${ffi_impls})
    else()
        set(cur_impls ${impls})
    endif()
    foreach(impl ${cur_impls})
        if(YUNI_${impl})
            set(cfg ${YUNIBASE_YUNIFIED_PATH}/gen-${IMPL_${impl}_gen}${scriptextgen})
            set(run ${dirname}/run-${IMPL_${impl}_sym}${scriptext})
            add_test(NAME "app-${dir}-${impl}"
                COMMAND ${CMAKE_COMMAND} 
                -DAPPDIR=${appdir}
                -DGEN=${cfg}
                -DRUN=${run}
                -DWORKDIR=${dirname}
                "${SH}"
                -P
                ${CMAKE_CURRENT_LIST_DIR}/TestRunner.cmake)
        endif()
    endforeach()
endfunction()

function(add_apptest dir)
    add_apptest0(${dir} FALSE)
endfunction()

function(add_ffitest dir)
    add_apptest0(${dir} TRUE)
endfunction()

add_apptest(hello)
add_ffitest(ffitrivial)

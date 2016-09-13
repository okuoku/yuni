set(yuniroot ${CMAKE_CURRENT_LIST_DIR}/../..)

include(${yuniroot}/lib-runtime/lib-runtime.cmake)

add_custom_target(yuni_bootstrap)

set(bootstrap_scripts
    ${CMAKE_CURRENT_LIST_DIR}/build-config-to-cmake-r7.sps
    ${CMAKE_CURRENT_LIST_DIR}/build-libmeta-to-cmake-r7.sps
    ${CMAKE_CURRENT_LIST_DIR}/build-config-to-cmake-r6.sps
    ${CMAKE_CURRENT_LIST_DIR}/build-libmeta-to-cmake-r6.sps)

# Detect bootstrap scheme
if(NOT YUNI_BOOTSTRAP_USE)
    if(YUNI_CHIBI_SCHEME)
        set(YUNI_BOOTSTRAP_USE chibi-scheme)
    elseif(YUNI_GOSH)
        set(YUNI_BOOTSTRAP_USE gauche)
    elseif(YUNI_SAGITTARIUS)
        set(YUNI_BOOTSTRAP_USE sagittarius)
    elseif(YUNI_IRON_SCHEME)
        set(YUNI_BOOTSTRAP_USE ironscheme)
    elseif(YUNI_RACKET)
        set(YUNI_BOOTSTRAP_USE racket)
    else()
        message(FATAL_ERROR "Failed to detect bootstrap scheme...")
    endif()
endif()

if(${YUNI_BOOTSTRAP_USE} STREQUAL "gauche")
    set(YUNI_BOOTSTRAP_SCHEME "${YUNI_GOSH}")
    set(YUNI_BOOTSTRAP_SCHEME_TYPE gauche)
elseif(${YUNI_BOOTSTRAP_USE} STREQUAL "sagittarius")
    set(YUNI_BOOTSTRAP_SCHEME "${YUNI_SAGITTARIUS}")
    set(YUNI_BOOTSTRAP_SCHEME_TYPE r7rs)
elseif(${YUNI_BOOTSTRAP_USE} STREQUAL "chibi-scheme")
    set(YUNI_BOOTSTRAP_SCHEME "${YUNI_CHIBI_SCHEME}")
    set(YUNI_BOOTSTRAP_SCHEME_TYPE r7rs)
elseif(${YUNI_BOOTSTRAP_USE} STREQUAL "ironscheme")
    set(YUNI_BOOTSTRAP_SCHEME "${YUNI_IRON_SCHEME}")
    set(YUNI_BOOTSTRAP_SCHEME_TYPE r6rs)
elseif(${YUNI_BOOTSTRAP_USE} STREQUAL "racket")
    set(YUNI_BOOTSTRAP_SCHEME "${YUNI_RACKET}")
    set(YUNI_BOOTSTRAP_SCHEME_TYPE racket)
else()
    message(FATAL_ERROR "Bootstrap Scheme was not found...")
endif()

set(bootparams
    "-DBOOTSTRAP=${YUNI_BOOTSTRAP_SCHEME}"
    -DBOOTSTRAP_TYPE=${YUNI_BOOTSTRAP_SCHEME_TYPE}
    -DYUNIROOT=${yuniroot}
    -DBUILDROOT=${CMAKE_CURRENT_BINARY_DIR}
    -DSTUBROOT=${CMAKE_CURRENT_BINARY_DIR}/lib-stub
    -DRUNTIMEROOT=${YUNIBASE_YUNIFIED_PATH}/runtime
    -P ${CMAKE_CURRENT_LIST_DIR}/YuniBootstrap.cmake)

# Run First bootstrap step
execute_process(
    COMMAND ${CMAKE_COMMAND} 
    -DOP=BOOTSTRAP_FIRST
    ${bootparams})

# Hook-up configuration dependencies
configure_file(
    ${CMAKE_CURRENT_LIST_DIR}/YuniBootstrap.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/YuniBootstrap_.cmake.current
    COPYONLY)

configure_file(
    ${CMAKE_CURRENT_LIST_DIR}/../../config/config.scm
    ${CMAKE_CURRENT_BINARY_DIR}/config.scm.current
    COPYONLY)

configure_file(
    ${CMAKE_CURRENT_BINARY_DIR}/yunilibs.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/yunilibs.cmake.current
    COPYONLY)

# Instantiate yuni library parameters
include(${CMAKE_CURRENT_BINARY_DIR}/yunilibs.cmake.current)
include(${CMAKE_CURRENT_BINARY_DIR}/libgenorder.cmake)
include(${CMAKE_CURRENT_BINARY_DIR}/genmappings.cmake)

# Instantiate checker
add_custom_target(yuni_bootstrap_check
    ${CMAKE_COMMAND}
    -DOP=BOOTSTRAP_CHECK
    ${bootparams}
    COMMENT "Checking source tree...")

# Instantiate stub library builds
set(outdata)
foreach(sym ${libgenorder}) # from libgenorder.cmake
    set(src ${libgenorder_${sym}_SOURCE})
    if(NOT EXISTS ${yuniroot}/${src})
        message(FATAL_ERROR 
            "Order list unmatched(Run cmake again): ${sym} ${src}")
    endif()
    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/libdata/${sym}.cmake
        COMMAND ${CMAKE_COMMAND}
        -DOP=GENLIBSTUB
        -DGENLIBSTUB_FILE=${src}
        ${bootparams}
        COMMENT "Yuni bootstrap... (${sym})"
        DEPENDS ${yuniroot}/${src}
        ${CMAKE_CURRENT_LIST_DIR}/YuniBootstrap.cmake
        ${bootstrap_scripts})
    # Deploy base .sls file into runtime dir
    foreach(e ${libgenorder_${sym}})
        if(${e} MATCHES "([^:]*):([^:]*):([^:]*)")
            set(impl ${CMAKE_MATCH_1})
            set(flav ${CMAKE_MATCH_2})
            set(top ${CMAKE_MATCH_3})
            string(REGEX REPLACE "[^/]+/(.*)"
                "${YUNIBASE_YUNIFIED_PATH}/runtime/${impl}/\\1" 
                out ${src})
            add_custom_command(
                OUTPUT ${out}
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                ${yuniroot}/${src} ${out}
                DEPENDS ${yuniroot}/${src})
            list(APPEND outdata ${out})
        endif()
    endforeach()
    list(APPEND outdata ${CMAKE_CURRENT_BINARY_DIR}/libdata/${sym}.cmake)
endforeach()
add_custom_target(yuni_stublibs ALL DEPENDS ${outdata})
add_dependencies(yuni_stublibs yuni_bootstrap_check)

add_dependencies(yuni_bootstrap yuni_stublibs)

# Generate impl => libdirlist
foreach(base GenRacket GenR7RS GenR6RSCommon)
    foreach(impl ${${base}})
        set(libdirlist_${impl} ${${base}-${impl}})
    endforeach()
endforeach()

# Instantiate runtime library deployment (for yunified)
function(deploy_impl_specific_runtime impl dirname)
    set(destfiles)
    if(lib_runtime_${impl})
        foreach(e ${lib_runtime_${impl}})
            set(in ${yuniroot}/lib-runtime/${e})
            set(out ${e})
            string(REGEX REPLACE "[^/]+/(.*)"
                "${YUNIBASE_YUNIFIED_PATH}/runtime/${dirname}/\\1" 
                out ${out})
            add_custom_command(
                OUTPUT ${out}
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                ${in} ${out}
                DEPENDS ${in})
            list(APPEND destfiles ${out})
        endforeach()
    endif()
    if(destfiles)
        add_custom_target(${impl}-runtime
            DEPENDS
            ${destfiles})
        add_dependencies(yuni_bootstrap ${impl}-runtime)
    endif()
endfunction()

macro(deploy_impl_specific_runtimes a b)
    deploy_impl_specific_runtime(${a} ${b})
    if(NOT "${ARGN}" STREQUAL "")
        deploy_impl_specific_runtimes(${ARGN})
    endif()
endmacro()

deploy_impl_specific_runtimes(
    CHIBI_SCHEME chibi
    GAUCHE       gauche
    GUILE        guile
    NMOSH        nmosh 
    RACKET       racket 
    SAGITTARIUS  sagittarius
    CHICKEN      chicken
    PICRIN       picrin
    KAWA         kawa
    RAPID_GAMBIT rapid-gambit
    LARCENY      larceny
    IRON_SCHEME  ironscheme
    CHEZ_SCHEME  chez
    VICARE       vicare
    GAMBIT       gambit
    MIT_SCHEME   mit-scheme
    )


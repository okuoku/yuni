add_custom_target(yuni_bootstrap)

set(bootstrap_scripts
    ${CMAKE_CURRENT_LIST_DIR}/build-config-to-cmake-r7.sps
    ${CMAKE_CURRENT_LIST_DIR}/build-libmeta-to-cmake-r7.sps)

# Detect bootstrap scheme
if(NOT YUNI_BOOTSTRAP_USE)
    if(YUNI_CHIBI_SCHEME)
        set(YUNI_BOOTSTRAP_USE chibi-scheme)
    elseif(YUNI_GOSH)
        set(YUNI_BOOTSTRAP_USE gauche)
    elseif(YUNI_SAGITTARIUS)
        set(YUNI_BOOTSTRAP_USE sagittarius)
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
else()
    message(FATAL_ERROR "Bootstrap Scheme was not found...")
endif()

set(yuniroot ${CMAKE_CURRENT_LIST_DIR}/../..)

set(bootparams
    "-DBOOTSTRAP=${YUNI_BOOTSTRAP_SCHEME}"
    -DBOOTSTRAP_TYPE=${YUNI_BOOTSTRAP_SCHEME_TYPE}
    -DYUNIROOT=${yuniroot}
    -DBUILDROOT=${CMAKE_CURRENT_BINARY_DIR}
    -DSTUBROOT=${CMAKE_CURRENT_BINARY_DIR}/lib-stub
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
    list(APPEND outdata ${CMAKE_CURRENT_BINARY_DIR}/libdata/${sym}.cmake)
endforeach()
add_custom_target(yuni_stublibs ALL DEPENDS ${outdata})
add_dependencies(yuni_stublibs yuni_bootstrap_check)

add_dependencies(yuni_bootstrap yuni_stublibs)

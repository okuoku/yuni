set(yuniroot ${YUNI_BASEDIR})

include(${yuniroot}/lib-runtime/lib-runtime.cmake)

add_custom_target(yuni_bootstrap)

set(bootstrap_scripts
    ${CMAKE_CURRENT_LIST_DIR}/build-config-to-cmake-r7.sps
    ${CMAKE_CURRENT_LIST_DIR}/build-libmeta-to-cmake-r7.sps
    ${CMAKE_CURRENT_LIST_DIR}/build-config-to-cmake-r6.sps
    ${CMAKE_CURRENT_LIST_DIR}/build-libmeta-to-cmake-r6.sps)

# Detect bootstrap scheme
if(NOT YUNI_BOOTSTRAP_USE)
    # Prefer Chez > Gauche > Sagittarius > ChibiScheme > Racket
    # Should match with yunibase name
    if(YUNI_CHEZ_SCHEME)
        set(YUNI_BOOTSTRAP_USE chez-scheme)
    elseif(YUNI_GOSH)
        set(YUNI_BOOTSTRAP_USE gauche)
    elseif(YUNI_SAGITTARIUS)
        set(YUNI_BOOTSTRAP_USE sagittarius)
    elseif(YUNI_CHIBI_SCHEME)
        set(YUNI_BOOTSTRAP_USE chibi-scheme)
    elseif(YUNI_RACKET)
        set(YUNI_BOOTSTRAP_USE racket)
    elseif(YUNI_IRON_SCHEME)
        set(YUNI_BOOTSTRAP_USE ironscheme)
    else()
        message(FATAL_ERROR "Failed to detect bootstrap scheme...")
    endif()
endif()

if(${YUNI_BOOTSTRAP_USE} STREQUAL "chez-scheme")
    set(YUNI_BOOTSTRAP_SCHEME "${YUNI_CHEZ_SCHEME}")
    set(YUNI_BOOTSTRAP_SCHEME_TYPE chez-scheme)
elseif(${YUNI_BOOTSTRAP_USE} STREQUAL "gauche")
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
    -DCONFIGROOT=${YUNI_BUILDCONFIG_LIBDIR}
    -P ${CMAKE_CURRENT_LIST_DIR}/YuniBootstrap.cmake)

# Generate (yuniconfig build) library
set(YUNIPLATFORM ${_PLATFORM})
set(YUNIRUNTIMEROOTPATH ${YUNIBASE_YUNIFIED_PATH}/runtime)
configure_file(
    ${CMAKE_CURRENT_LIST_DIR}/yuniconfig-build.sls.in
    ${YUNI_BUILDCONFIG_LIBDIR}/yuniconfig/build.sls
    @ONLY)

# Run First bootstrap step
message(STATUS "Generating library order list...")
execute_process(
    COMMAND ${CMAKE_COMMAND} 
    -DOP=BOOTSTRAP_FIRST
    ${bootparams})
message(STATUS "Done.")

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

# Generate yunilibs.scm to generate libmeta
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/yunilibs.scm
    "(\n")
foreach(e ${yunilibfiles})
    file(APPEND ${CMAKE_CURRENT_BINARY_DIR}/yunilibs.scm 
        "\"${e}\"\n")
endforeach()
file(APPEND ${CMAKE_CURRENT_BINARY_DIR}/yunilibs.scm ")\n")

# Generate libmeta.cmake
message(STATUS "Generating library database...")
execute_process(
    COMMAND ${CMAKE_COMMAND}
    -DGENLIBMETA_IN=${CMAKE_CURRENT_BINARY_DIR}/yunilibs.scm
    -DGENLIBMETA_OUT=${CMAKE_CURRENT_BINARY_DIR}/libmeta.cmake
    -DOP=GENLIBMETA
    ${bootparams}
    WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}/../..)
message(STATUS "Done.")

# Instantiate checker
add_custom_target(yuni_bootstrap_check ALL
    ${CMAKE_COMMAND}
    -DOP=BOOTSTRAP_CHECK
    ${bootparams}
    COMMENT "Checking source tree...")

# Instantiate stub library builds
set(outdata)
foreach(sym ${libgenorder}) # from libgenorder.cmake
    set(src ${libgenorder_${sym}_SOURCE})
    set(relsrc ${libgenorder_${sym}_RELSOURCE})
    if(NOT EXISTS ${src})
        message(FATAL_ERROR 
            "Order list unmatched(Run cmake again): ${sym} ${src}")
    endif()
    set(_done ${CMAKE_CURRENT_BINARY_DIR}/done/${sym}.done)
    set(stubs)
    # Add output arguments for Package build phase
    foreach(e ${libgenorder_${sym}})
        if(${e} MATCHES "([^:]*):([^:]*):([^:]*)")
            set(impl ${CMAKE_MATCH_1})
            set(top ${CMAKE_MATCH_3})
            set(orig ${YUNIBASE_YUNIFIED_PATH}/runtime/${impl}/${relsrc})
            # maybe_alias: Remove 1 entry and replace it with ${impl}/${top}
            string(REGEX REPLACE "[^/]+/(.*)"
                "${YUNIBASE_YUNIFIED_PATH}/runtime/${impl}/${top}/\\1" 
                maybe_alias ${relsrc})
            if(${impl} STREQUAL racket)
                # Replace extension
                string(REGEX REPLACE "\\.sls" ".mzscheme.sls"
                    orig ${orig})
                string(REGEX REPLACE "\\.sls" ".mzscheme.sls"
                    maybe_alias ${maybe_alias})
                #message(STATUS "We generate ${orig}")
                list(APPEND stubs ${orig})
                if(NOT ${orig} STREQUAL ${maybe_alias})
                    #message(STATUS "We too generate ${maybe_alias}")
                    list(APPEND stubs ${maybe_alias})
                endif()
            elseif(${impl} STREQUAL larceny)
                if(NOT ${orig} STREQUAL ${maybe_alias})
                    list(APPEND stubs ${maybe_alias})
                endif()
            endif()
        endif()
    endforeach()

    # Instantiate output command
    add_custom_command(
        OUTPUT ${_done} ${stubs}
        COMMAND ${CMAKE_COMMAND}
        -DOP=GENLIBSTUB
        -DGENLIBSTUB_FILE=${src}
        ${bootparams}
        COMMAND ${CMAKE_COMMAND} -E copy # Enforce touch
        ${src} ${_done}
        DEPENDS ${src}
        ${CMAKE_CURRENT_LIST_DIR}/YuniBootstrap.cmake
        ${bootstrap_scripts}
        COMMENT "Generate stub library... (${sym})")

    # Deploy base .sls file into runtime dir
    foreach(e ${libgenorder_${sym}})
        if(${e} MATCHES "([^:]*):([^:]*):([^:]*)")
            set(impl ${CMAKE_MATCH_1})
            set(flav ${CMAKE_MATCH_2})
            set(top ${CMAKE_MATCH_3})
            set(out ${YUNIBASE_YUNIFIED_PATH}/runtime/${impl}/${relsrc})
            add_custom_command(
                OUTPUT ${out} 
                COMMAND ${CMAKE_COMMAND} -E copy
                ${src} ${out}
                DEPENDS ${src})
            # message(STATUS "Deploy ${out}")
            list(APPEND outdata ${out})
        endif()
    endforeach()
    list(APPEND outdata ${_done})
endforeach()
add_custom_target(yuni_stublibs ALL DEPENDS ${outdata})
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
                # We could copy_if_different here but just wanted ensure
                # "touch" will cause rebuild
                COMMAND ${CMAKE_COMMAND} -E copy
                ${in} ${out}
                DEPENDS ${in})
            # message(STATUS "Runtime deploy: ${in} => ${out}")
            list(APPEND destfiles ${out})
        endforeach()
    endif()
    if(destfiles)
        add_custom_target(${impl}-runtime ALL
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
    CHIBI_SCHEME chibi-scheme
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

# Decided bootstrap scheme impl.
set(YUNI_BOOTSTRAP_USE "${YUNI_BOOTSTRAP_USE}" PARENT_SCOPE)

# Generate runner
emit_yuni_runners()

include(${CMAKE_CURRENT_LIST_DIR}/package.cmake)

set(yuniroot ${YUNI_BASEDIR})

include(${yuniroot}/lib-runtime/lib-runtime.cmake)

add_custom_target(yuni_bootstrap)

set(bootstrap_scripts
    ${CMAKE_CURRENT_LIST_DIR}/build-config-to-cmake-r7.sps
    ${CMAKE_CURRENT_LIST_DIR}/build-libmeta-to-cmake-r7.sps
    ${CMAKE_CURRENT_LIST_DIR}/build-config-to-cmake-r6.sps
    ${CMAKE_CURRENT_LIST_DIR}/build-libmeta-to-cmake-r6.sps)

yunidetectscheme_guess_bootstrap()

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
set(YUNILOADERROOTPATH ${YUNIBASE_YUNIFIED_PATH}/loader)
set(YUNIRUNTIMEROOTPATH ${YUNIBASE_YUNIFIED_PATH}/runtime)
set(YUNIPLATFORMSTUBDIR ${YUNI_PLATFORM_LIBDIR})
set(YUNIPLATFORMSTUBDIRNAME ${YUNI_PLATFORM_LIBDIRNAME})

function(genruntime libname src dest)
    set(out ${YUNI_BUILDCONFIG_LIBDIR}/${dest})
    set(srcfile ${CMAKE_CURRENT_LIST_DIR}/${src})
    configure_file(${srcfile} ${out} @ONLY)
    set(libaltsrc_${libname} ${srcfile} PARENT_SCOPE)
endfunction()


macro(setyuniconfigtarget nam)
    # FIXME: How to handle compilers??
    if(${nam} STREQUAL GAUCHE)
        set(_exec GOSH)
    elseif(${nam} STREQUAL CHICKEN)
        set(_exec CHICKEN_CSI)
    elseif(${nam} STREQUAL GAMBIT)
        set(_exec GSI)
    elseif(${nam} STREQUAL KAWA)
        set(_exec KAWA_JAR)
    elseif(${nam} STREQUAL BIWASCHEME)
        set(_exec BIWASYUNI)
    else()
        set(_exec ${nam})
    endif()

    if(YUNI_${_exec})
        set(YUNICONFIG${nam} "${YUNI_${_exec}}")
    else()
        set(YUNICONFIG${nam} "UNKNOWN")
    endif()
endmacro()
foreach(e GAUCHE CHIBI_SCHEME PICRIN KAWA SAGITTARIUS RACKET GUILE
        CHICKEN
        LARCENY IRON_SCHEME CHEZ_SCHEME VICARE NMOSH GAMBIT MIT_SCHEME S7
        BIWASCHEME)
    setyuniconfigtarget(${e})
endforeach()

genruntime(yuniconfig_build yuniconfig-build.sls.in 
    yuniconfig/build.sls)

# Generate (yuniexternal alexpander) library
set(YUNIALEXPANDER ${YUNI_BASEDIR}/external/yuni-alexpander.scm)
file(READ ${YUNIALEXPANDER} YUNI_ALEXPANDER_SOURCE)
## Replace $$yunifake-hook => $$yunifake-hook0
string(REGEX REPLACE "yunifake-hook" "yunifake-hook0"
    YUNI_ALEXPANDER_SOURCE
    "${YUNI_ALEXPANDER_SOURCE}")
configure_file(${YUNIALEXPANDER} 
    ${CMAKE_CURRENT_BINARY_DIR}/yuni-alexpander.scm.current
    COPYONLY)

genruntime(yuniexternal_alexpander 
    yuniexternal-alexpander.sls.in
    yuniexternal/alexpander.sls)

# Run First bootstrap step
message(STATUS "Generating library order list...")
execute_process(
    COMMAND ${CMAKE_COMMAND} 
    -DOP=BOOTSTRAP_FIRST
    ${bootparams}
    RESULT_VARIABLE rr)
if(rr)
    message(FATAL_ERROR "Failed (${rr})")
endif()
message(STATUS "Done.")

# Hook-up configuration dependencies
configure_file(
    ${CMAKE_CURRENT_LIST_DIR}/YuniBootstrap.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/YuniBootstrap_.cmake.current
    COPYONLY)

configure_file(
    ${yuniroot}/config/config.scm
    ${CMAKE_CURRENT_BINARY_DIR}/config.scm.current
    COPYONLY)

configure_file(
    ${CMAKE_CURRENT_BINARY_DIR}/yunilibs.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/yunilibs.cmake.current
    COPYONLY)

# Instantiate yuni library parameters
if(NOT EXISTS ${CMAKE_CURRENT_BINARY_DIR}/libgenorder.cmake)
    message(FATAL_ERROR "libgenorder not generated.")
endif()
if(NOT EXISTS ${CMAKE_CURRENT_BINARY_DIR}/genmappings.cmake)
    message(FATAL_ERROR "genmappings not generated.")
endif()
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
    WORKING_DIRECTORY ${yuniroot}
    RESULT_VARIABLE rr)
if(rr)
    message(FATAL_ERROR "Failed (${rr})")
endif()
message(STATUS "Done.")

# Instantiate checker
add_custom_target(yuni_bootstrap_check ALL
    ${CMAKE_COMMAND}
    -DOP=BOOTSTRAP_CHECK
    ${bootparams}
    DEPENDS yuniruntime-copy 
    COMMENT "Checking source tree...")

# Instantiate depsrc
foreach(sym ${libgenorder})
    if(libaltsrc_${sym})
        set(depsrc_${sym} ${libaltsrc_${sym}})
    else()
        set(depsrc_${sym} ${libgenorder_${sym}_SOURCE})
    endif()
endforeach()

# Instantiate stub library builds
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/done)
set(outdata)
foreach(sym ${libgenorder}) # from libgenorder.cmake
    set(src ${libgenorder_${sym}_SOURCE})
    set(depsrc ${depsrc_${sym}})
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

    # Deploy base .sls file into runtime dir
    set(copyrequests)
    set(copyoutputs)
    foreach(e ${libgenorder_${sym}})
        if(${e} MATCHES "([^:]*):([^:]*):([^:]*)")
            set(impl ${CMAKE_MATCH_1})
            set(flav ${CMAKE_MATCH_2})
            set(top ${CMAKE_MATCH_3})
            set(out ${YUNIBASE_YUNIFIED_PATH}/runtime/${impl}/${relsrc})
            list(APPEND copyoutputs ${out})
            list(APPEND copyrequests 
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                ${src} ${out})
        endif()
    endforeach()

    # Instantiate output command
    add_custom_command(
        OUTPUT ${_done} ${stubs} ${copyoutputs}
        COMMAND ${CMAKE_COMMAND}
        -DOP=GENLIBSTUB
        -DGENLIBSTUB_FILE=${src}
        ${bootparams}
        # NB: Make sure keep always up-to-date {_done} file
        COMMAND ${CMAKE_COMMAND} -E copy # Enforce touch
        ${src} ${_done}
        ${copyrequests}
        DEPENDS ${depsrc}
        ${CMAKE_CURRENT_LIST_DIR}/YuniBootstrap.cmake
        ${bootstrap_scripts}
        COMMENT "Generate stub library... (${sym})")
    list(APPEND outdata ${_done})
endforeach()
add_custom_target(yuni_stublibs DEPENDS ${outdata})
add_dependencies(yuni_bootstrap yuni_stublibs)

# Generate impl => libdirlist
foreach(base GenRacket GenR7RS GenR6RSCommon)
    foreach(impl ${${base}})
        set(libdirlist_${impl} ${${base}-${impl}})
    endforeach()
endforeach()

# Instantiate runtime library deployment (for yunified)
macro(deploy_impl_specific_runtime impl dirname)
    set(destfiles)
    if(lib_runtime_${impl})
        foreach(e ${lib_runtime_${impl}})
            string(REPLACE / _ base ${e})
            set(done ${CMAKE_CURRENT_BINARY_DIR}/done/impl-${impl}-${base})
            set(in ${yuniroot}/lib-runtime/${e})
            set(out ${e})
            string(REGEX REPLACE "[^/]+/(.*)"
                "${YUNIBASE_YUNIFIED_PATH}/runtime/${dirname}/\\1" 
                out ${out})
            add_custom_command(
                OUTPUT ${done} ${out}
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                ${in} ${out}
                COMMAND ${CMAKE_COMMAND} -E touch ${done}
                COMMENT "Copying impl-runtime: ${e}"
                DEPENDS ${in})
            # message(STATUS "Runtime deploy: ${in} => ${out}")
            list(APPEND destfiles ${done})
        endforeach()
    endif()
    if(destfiles)
        list(APPEND generated_runtime_files ${destfiles})
        set(generated_runtime_files ${generated_runtime_files} PARENT_SCOPE)
    endif()
endmacro()

macro(deploy_impl_specific_runtimes a b)
    deploy_impl_specific_runtime(${a} ${b})
    if(NOT "${ARGN}" STREQUAL "")
        deploy_impl_specific_runtimes(${ARGN})
    endif()
endmacro()

set(generated_runtime_files)

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

if(generated_runtime_files)
    add_custom_target(yuni_bootstrap-runtime
        DEPENDS
        ${generated_runtime_files})
    add_dependencies(yuni_bootstrap yuni_bootstrap-runtime)
endif()

# Decided bootstrap scheme impl.
set(YUNI_BOOTSTRAP_USE "${YUNI_BOOTSTRAP_USE}" PARENT_SCOPE)

# Generate runner
emit_yuni_runners()

include(${CMAKE_CURRENT_LIST_DIR}/genericruntime.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/package.cmake)

# Generate generic-runtime
add_gen_generic_runtime(genruntime-s7 s7 
    ${YUNIBASE_YUNIFIED_PATH}/runtime/s7/yuniruntime.scm)

add_gen_generic_runtime(genruntime-biwascheme biwascheme
    ${YUNIBASE_YUNIFIED_PATH}/runtime/biwascheme/yuniruntime.scm)

add_custom_target(genruntimes ALL DEPENDS 
    genruntime-biwascheme
    genruntime-s7)

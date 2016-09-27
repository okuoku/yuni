add_custom_target(yuni_package)
add_custom_target(yuni_package_build)

include(${YUNI_BASEDIR}/lib-runtime/lib-runtime.cmake)
include(${CMAKE_CURRENT_BINARY_DIR}/../bootstrap/libmeta.cmake)
include(${CMAKE_CURRENT_BINARY_DIR}/../bootstrap/libgenorder.cmake)

set(allsps ${CMAKE_CURRENT_BINARY_DIR}/../bootstrap/_yuniall.sps)

macro(check_impl var nam)
    if(WIN32)
        set(_pth ${YUNIBASE_YUNIFIED_PATH}/${nam}.bat)
    else()
        set(_pth ${YUNIBASE_YUNIFIED_PATH}/${nam})
    endif()
    if(EXISTS ${_pth})
        set(YUNIBUILD_${var} ${_pth})
        if(${nam} STREQUAL gosh)
            set(compnam gauche)
        else()
            set(compnam ${nam})
        endif()
        if(${compnam} STREQUAL ${YUNI_BOOTSTRAP_USE})
            set(YUNIBUILD_${var}_IS_BOOTSTRAP TRUE)
        endif()
    endif()
endmacro()

check_impl(CHIBI_SCHEME chibi-scheme)
check_impl(GOSH gosh)
check_impl(GUILE guile)
check_impl(RACKET racket)
check_impl(SAGITTARIUS sagittarius)
check_impl(CSI csi)
check_impl(VICARE vicare)
check_impl(NMOSH nmosh)
check_impl(KAWA kawa)
check_impl(LARCENY larceny)
check_impl(PETITE_CHEZ_SCHEME petite-chez-scheme)
check_impl(CHEZ_SCHEME chez-scheme)
check_impl(GSI gsi)
check_impl(RAPID_GAMBIT rapid-gambit)
check_impl(PICRIN picrin)
check_impl(MIT_SCHEME mit-scheme)
check_impl(IRON_SCHEME ironscheme)

# 
# Pre-Cache: Guile, Sagittarius, nmosh
#

function(check_bootstrap impl tgt)
    if(YUNIBUILD_${impl}_IS_BOOTSTRAP)
        add_dependencies(yuni_package_build ${tgt})
    endif()
endfunction()


function(precache impl)
    if(YUNIBUILD_${impl})
        add_custom_target(yuni_precache_${impl} ALL
            ${YUNIBUILD_${impl}} ${ARGN} ${allsps}
            COMMENT "Precache(${impl})")
        add_dependencies(yuni_precache_${impl} yuni_bootstrap)
        check_bootstrap(${impl} yuni_precache_${impl})
    endif()
endfunction()

precache(GUILE)
# precache(NMOSH)
precache(SAGITTARIUS)

#
# Library-by-Library Compile: Racket, Larceny
#

set(compile_impls)
if(YUNIBUILD_RACKET)
    list(APPEND compile_impls racket)
endif()
if(YUNIBUILD_LARCENY)
    list(APPEND compile_impls larceny)
endif()

## Phase1: Construct libsym_impl to depfiles table
##   libs_<impl>_<libsym>_file     => path to main file
##   libs_<impl>_<libsym>_deplibs  => libsym for dependency
##   libs_<impl>_<libsym>_alias_of => libsym to original
foreach(sym ${libgenorder})
    set(relsrc ${libgenorder_${sym}_SOURCE})
    if(${sym} MATCHES "([^_]*)_(.*)")
        set(orighead ${CMAKE_MATCH_1})
        set(origrest ${CMAKE_MATCH_2})
    else()
        message(FATAL_ERROR "invalid libsym ${sym}")
    endif()

    foreach(e ${libgenorder_${sym}})
        if(${e} MATCHES "([^:]*):([^:]*):([^:]*)")
            set(impl ${CMAKE_MATCH_1})
            set(flav ${CMAKE_MATCH_2})
            set(top ${CMAKE_MATCH_3})
            string(REGEX REPLACE "[^/]+/(.*)"
                "${YUNIBASE_YUNIFIED_PATH}/runtime/${impl}/\\1"
                src ${relsrc})
            set(libs_${impl}_${sym}_file ${src})
            if(libdeps_${sym})
                set(deps ${libdeps_${sym}})
                # Racket workaround
                if(${impl} STREQUAL racket)
                    set(deps)
                    foreach(e ${libdeps_${sym}})
                        if(${e} STREQUAL scheme_base)
                            set(e scheme_base0)
                        elseif(${e} STREQUAL scheme_file)
                            set(e scheme_file0)
                        endif()
                        list(APPEND deps ${e})
                    endforeach()
                endif()
                set(libs_${impl}_${sym}_deplibs ${deps})
            endif()
            if(NOT ${top} STREQUAL ${orighead})
                # Racket workaround
                set(aliasname ${top}_${origrest})
                if(${impl} STREQUAL racket)
                    if(${aliasname} STREQUAL scheme_base)
                        set(aliasname scheme_base0)
                    elseif(${aliasname} STREQUAL scheme_file)
                        set(aliasname scheme_file0)
                    endif()
                endif()
                set(libs_${impl}_${sym}_alias_of ${aliasname})
                # Instantiate static dependency: alias => orig
                set(libs_${impl}_${aliasname}_deplibs
                    ${sym})
            endif()
        endif()
    endforeach()
endforeach()

add_custom_target(yunipreroll_racket)

function(calc_larceny_output_name var src)
    get_filename_component(srcbase ${src} PATH)
    get_filename_component(srcname ${src} NAME_WE)
    set(cachefile ${srcbase}/${srcname}.slfasl)
    set(${var} ${cachefile} PARENT_SCOPE)
endfunction()

function(larceny_compile tgt src) # ARGN = deps
    calc_larceny_output_name(cachefile ${src})
    file(TO_NATIVE_PATH ${src} nsrc)
    set(_script ${CMAKE_CURRENT_LIST_DIR}/_larceny_compile.sps)
    add_custom_command(
        OUTPUT ${cachefile}
        COMMAND ${YUNIBUILD_LARCENY} ${_script} ${nsrc}
        DEPENDS ${src} ${_script} yuni_bootstrap  ${ARGN}
        COMMENT "Compile(Larceny, ${tgt})...")
    add_custom_target(${tgt} ALL
        DEPENDS ${cachefile})
    # message(STATUS "Larceny compile(${tgt}): ${src}")
    # foreach(e ${ARGN} ${src})
    #     message(STATUS "    ${e}")
    # endforeach()
endfunction()

function(calc_racket_output_name var src)
    get_filename_component(srcbase ${src} PATH)
    get_filename_component(srcname ${src} NAME_WE)
    set(cachefile ${srcbase}/compiled/${srcname}.mzscheme_sls.zo)
    set(${var} ${cachefile} PARENT_SCOPE)
endfunction()

function(racket_compile_dep dep tgt src) # ARGN = deps
    calc_racket_output_name(cachefile ${src})
    add_custom_command(
        OUTPUT ${cachefile}
        COMMAND ${YUNIBUILD_RACKET} --compile ${src}
        DEPENDS ${src} yuni_bootstrap ${dep} ${ARGN}
        COMMENT "Compile(Racket, ${tgt})...")
    add_custom_target(${tgt} ALL
        DEPENDS ${cachefile})
endfunction()

function(racket_compile_preroll tgt src)
    racket_compile_dep("" ${tgt} ${src} ${ARGN})
endfunction()

function(racket_compile tgt src)
    racket_compile_dep(yunipreroll_racket ${tgt} ${src} ${ARGN})
endfunction()

function(calc_depoutputs var impl tgt)
    set(out)
    set(deps ${libs_${impl}_${tgt}_deplibs})
    foreach(sym ${deps})
        if(NOT yunioutput_${impl}_${sym})
            message(STATUS "Dep not found(${impl}) for ${tgt}: ${sym}")
        else()
            # message(STATUS "Dep ${impl} ${tgt} += ${yunioutput_${impl}_${sym}}(${sym})")
        endif()
        list(APPEND out ${yunioutput_${impl}_${sym}})
        if(libs_${impl}_${sym}_alias_of)
            set(origin_sym ${libs_${impl}_${sym}_alias_of})
            set(origin_output ${yunioutput_${impl}_${origin_sym}})
            # message(STATUS "DepAlias ${impl} ${tgt} += ${origin_output}")
            list(APPEND out ${origin_output})
        endif()
    endforeach()
    set(${var} ${out} PARENT_SCOPE)
endfunction()

## Phase2: Instantiate build rules
##      yunicompile_tgt_<impl> => list of tgts
foreach(impl ${compile_impls})
    set(yunicompile_tgt_${impl})
    ## Phase2.0: Calc output name for libsyms
    foreach(sym ${libgenorder})
        set(request_build OFF)
        set(request_build OFF)
        foreach(e ${libgenorder_${sym}})
            if(${e} MATCHES "([^:]*):([^:]*):([^:]*)")
                set(buildimpl ${CMAKE_MATCH_1})
                if(${buildimpl} STREQUAL ${impl})
                    set(request_build ON)
                endif()
            endif()
        endforeach()
        if(request_build)
            if(${impl} STREQUAL racket)
                # convert source path .sls => .mzscheme.sls
                set(basepath ${libs_${impl}_${sym}_file})
                string(REGEX REPLACE "\\.sls" ".mzscheme.sls"
                    src ${basepath})
                calc_racket_output_name(yunioutput_${impl}_${sym} ${src})
            elseif(${impl} STREQUAL larceny)
                set(src ${libs_${impl}_${sym}_file})
                calc_larceny_output_name(yunioutput_${impl}_${sym} ${src})
            endif()

            # Calc output name for alias libraries
            if(libs_${impl}_${sym}_alias_of)
                set(tgt ${libs_${impl}_${sym}_alias_of})
                # message(STATUS "Alias ${tgt} (from ${impl} ${sym})")
                if(${impl} STREQUAL racket)
                    # Calc alias path using libsym
                    string(REGEX REPLACE "_" "/" pthbase ${tgt})
                    set(aliassrc ${YUNIBASE_YUNIFIED_PATH}/runtime/racket/${pthbase}.mzscheme.sls)
                    calc_racket_output_name(yunioutput_${impl}_${tgt} ${aliassrc})
                elseif(${impl} STREQUAL larceny)
                    set(src ${libs_${impl}_${sym}_file})
                    # Calc alias path using libsym
                    string(REGEX REPLACE "_" "/" pthbase ${tgt})
                    set(aliassrc ${YUNIBASE_YUNIFIED_PATH}/runtime/larceny/${pthbase}.sls)
                    calc_larceny_output_name(yunioutput_${impl}_${tgt} ${aliassrc})
                endif()
            endif()
        endif()
    endforeach()

    ## Phase2.1: Instantiate actual build rules
    ##  yunicompile_<impl>_<libsym>
    foreach(sym ${libgenorder})
        set(request_build OFF)
        foreach(e ${libgenorder_${sym}})
            if(${e} MATCHES "([^:]*):([^:]*):([^:]*)")
                set(buildimpl ${CMAKE_MATCH_1})
                if(${buildimpl} STREQUAL ${impl})
                    set(request_build ON)
                endif()
            endif()
        endforeach()
        if(request_build)
            if(${impl} STREQUAL racket)
                # convert source path .sls => .mzscheme.sls
                set(basepath ${libs_${impl}_${sym}_file})
                string(REGEX REPLACE "\\.sls" ".mzscheme.sls"
                    src ${basepath})
                calc_depoutputs(deps ${impl} ${sym})
                racket_compile(yunicompile_${impl}_${sym} ${src} 
                    ${deps})
                list(APPEND yunicompile_tgt_${impl} yunicompile_${impl}_${sym})
                check_bootstrap(${impl} yunicompile_${impl}_${sym})
                # message(STATUS "Build: ${src} => yunicompile_${impl}_${sym}")
                if(libs_${impl}_${sym}_alias_of)
                    # Add alias target too here.
                    set(tgt ${libs_${impl}_${sym}_alias_of})
                    calc_racket_output_name(origout ${src})
                    # Calc alias path using libsym
                    string(REGEX REPLACE "_" "/" pthbase ${tgt})
                    set(aliassrc ${YUNIBASE_YUNIFIED_PATH}/runtime/racket/${pthbase}.mzscheme.sls)
                    racket_compile(yunicompile_${impl}_${tgt}
                        ${aliassrc} ${origout})
                    list(APPEND yunicompile_tgt_${impl} 
                        yunicompile_${impl}_${tgt})
                    # message(STATUS "Alias Build: ${aliassrc} => yunicompile_${impl}_${tgt}")
                endif()
            elseif(${impl} STREQUAL larceny)
                set(src ${libs_${impl}_${sym}_file})
                calc_depoutputs(deps ${impl} ${sym})
                larceny_compile(yunicompile_${impl}_${sym} ${src} ${deps})
                list(APPEND yunicompile_tgt_${impl}
                    yunicompile_${impl}_${sym})
                if(libs_${impl}_${sym}_alias_of)
                    # Add alias target too here.
                    set(tgt ${libs_${impl}_${sym}_alias_of})
                    calc_larceny_output_name(origout ${src})
                    # Calc alias path using libsym
                    string(REGEX REPLACE "_" "/" pthbase ${tgt})
                    set(aliassrc ${YUNIBASE_YUNIFIED_PATH}/runtime/larceny/${pthbase}.sls)
                    larceny_compile(yunicompile_${impl}_${tgt}
                        ${aliassrc} ${origout})
                    list(APPEND yunicompile_tgt_${impl} 
                        yunicompile_${impl}_${tgt})
                endif()
            endif()
        endif()
    endforeach()
endforeach()

# Phase3: Preroll/Fixup
# FIXME: We should have this phase in front of phase2.x instead
#        (Currently we don't have to do so because Racket fixup should
#         take care dependencies too)
foreach(impl ${compile_impls})
    if(${impl} STREQUAL racket)
        foreach(fil ${lib_runtime_RACKET})
            get_filename_component(nam ${fil} NAME_WE)
            set(tgt yunipreroll-racket-${nam})
            string(REGEX REPLACE "[^/]+/(.*)"
                "${YUNIBASE_YUNIFIED_PATH}/runtime/${impl}/\\1"
                src ${fil})
            racket_compile_preroll(${tgt} ${src})
            add_dependencies(yunipreroll_racket ${tgt})
        endforeach()
        add_custom_target(yunifixup_racket ALL
            ${YUNIBUILD_RACKET} --compile ${allsps}
            COMMENT "Fixup(Racket)")
        add_dependencies(yunifixup_racket
            ${yunicompile_tgt_racket})
    endif()
endforeach()

# precache(RACKET --compile)
# FIXME: Not working.
#precache(VICARE --build-directory ${YUNIBASE_YUNIFIED_PATH}/runtime/vicare
#    --compile-dependencies)

#
# YuniBootstrap: Common code for bootstrap
#
# INPUTs:
#  OP: BOOTSTRAP_FIRST | BOOTSTRAP_CHECK | GENLIBSTUB | GENLIBMETA
#  BOOTSTRAP: Full path to Bootstrap Scheme
#  BOOTSTRAP_TYPE: CHIBI_SCHEME
#  YUNIROOT: Full path to yunisource
#  BUILDROOT: Full path to metadata
#  STUBROOT: Full path to stublib
#  RUNTIMEROOT: Full path to runtime
#  CONFIGROOT: Full path to config-time generated libraries
#
#  GENLIBSTUB_FILE: Relative path against YUNIROOT for GENLIBSTUB
#
#  GENLIBMETA_IN: Input for yunilibs.scm (Relative path list)
#  GENLIBMETA_OUT: Output for libmeta.cmake
#
#

function(select_script_file var slot)
    set(config_to_cmake_r7 
        ${CMAKE_CURRENT_LIST_DIR}/build-config-to-cmake-r7.sps)
    set(libmeta_to_cmake_r7
        ${CMAKE_CURRENT_LIST_DIR}/build-libmeta-to-cmake-r7.sps)
    set(config_to_cmake_r6 
        ${CMAKE_CURRENT_LIST_DIR}/build-config-to-cmake-r6.sps)
    set(libmeta_to_cmake_r6
        ${CMAKE_CURRENT_LIST_DIR}/build-libmeta-to-cmake-r6.sps)

    set(script_type r7)
    if(${BOOTSTRAP_TYPE} STREQUAL racket)
        set(script_type r6)
    elseif(${BOOTSTRAP_TYPE} STREQUAL r6rs)
        set(script_type r6)
    elseif(${BOOTSTRAP_TYPE} STREQUAL chez-scheme)
        set(script_type r6)
    endif()

    if(${slot} STREQUAL CONFIG_TO_CMAKE)
        set(out ${config_to_cmake_${script_type}})
    elseif(${slot} STREQUAL LIBMETA_TO_CMAKE)
        set(out ${libmeta_to_cmake_${script_type}})
    else()
        message(FATAL_ERROR "Unknown script type: ${slot}")
    endif()

    set(${var} ${out} PARENT_SCOPE)
endfunction()

function(bootstrap_run_scheme slot) # ARGN = args
    select_script_file(script ${slot})

    if(${BOOTSTRAP_TYPE} STREQUAL "gauche")
        set(addargs -r7)
    elseif(${BOOTSTRAP_TYPE} STREQUAL "racket")
        set(addargs 
            -I scheme/init -l- r6rs/run.rkt)
    elseif(${BOOTSTRAP_TYPE} STREQUAL "chez-scheme")
        set(addargs --program)
    else()
        set(addargs)
    endif()
    execute_process(
        COMMAND "${BOOTSTRAP}" ${addargs} ${script} ${ARGN}
        WORKING_DIRECTORY ${BUILDROOT}
        RESULT_VARIABLE rr)
    if(rr)
        message(FATAL_ERROR "Run scheme error(${rr}): ${script}")
    endif()
endfunction()

set(libdirs
    "lib"
    "lib-r6rs"
    "lib-compat")

function(bootstrap_collect_sls var) # Generate library file list
    set(sls)
    foreach(e ${libdirs})
        file(GLOB_RECURSE fns
            ${YUNIROOT}/${e}/*.sls)
        list(APPEND sls ${fns})
    endforeach()
    # Add configroot
    file(GLOB_RECURSE fns ${CONFIGROOT}/*.sls)
    list(APPEND sls ${fns})
    set(${var} ${sls} PARENT_SCOPE)
endfunction()

function(bootstrap_gen_yunilibfiles nam)
    # Generate yunilibfiles.cmake
    bootstrap_collect_sls(libs)
    file(WRITE ${BUILDROOT}/${nam} "set(yunilibfiles\n")
    foreach(e ${libs})
        file(APPEND ${BUILDROOT}/${nam} "  \"${e}\"\n")
    endforeach()
    file(APPEND ${BUILDROOT}/${nam} ")\n")
endfunction()

function(bootstrap_libsym_split first rest sym)
    if(${sym} MATCHES "([^_]*)_(.*)")
        set(${first} ${CMAKE_MATCH_1} PARENT_SCOPE)
        set(${rest} ${CMAKE_MATCH_2} PARENT_SCOPE)
    else()
        set(${first} ${sym} PARENT_SCOPE)
        set(${rest} "" PARENT_SCOPE)
    endif()
endfunction()

function(bootstrap_libmap_split from to nam)
    if(${nam} MATCHES "([^']*)'(.*)")
        set(${from} ${CMAKE_MATCH_1} PARENT_SCOPE)
        set(${to} ${CMAKE_MATCH_2} PARENT_SCOPE)
    else()
        set(${from} ${nam} PARENT_SCOPE)
        set(${to} ${nam} PARENT_SCOPE)
    endif()
endfunction()

function(bootstrap_libpath_strip var pth)
    set(rootname)
    foreach(root ${libdirs})
        if(${pth} MATCHES "${YUNIROOT}/${root}/([^.]*).*")
            set(rootname ${CMAKE_MATCH_1})
            break()
        endif()
    endforeach()

    if(NOT rootname)
        # Second chance for configroot
        if(${pth} MATCHES "${CONFIGROOT}/([^.]*).*")
            set(rootname ${CMAKE_MATCH_1})
        else()
            message(FATAL_ERROR "Unknown rootpath for .sls ${pth}")
        endif()
    endif()

    set(${var} ${rootname} PARENT_SCOPE)
endfunction()

function(bootstrap_path_to_libsym var pth)
    bootstrap_libpath_strip(rootname ${pth})
    string(REGEX REPLACE "/" "_" rootname ${rootname})
    set(${var} ${rootname} PARENT_SCOPE)
endfunction()

function(bootstrap_filter_includepath inoutvar impl)
    set(input ${${inoutvar}})
    if(${impl} STREQUAL chibi-scheme)
        # Abuse moudle-7 behaviour: strip down to basename
        get_filename_component(out ${${inoutvar}} NAME)
        set(${inoutvar} ${out} PARENT_SCOPE)
    elseif(${impl} STREQUAL kawa)
        # Use absolute paths for kawa
        set(pth ${input})
        set(${inoutvar} ${pth} PARENT_SCOPE)
    endif()
endfunction()

function(bootstrap_gen_librequest)
    # Generate an "order table" for each library files
    # libgenorder = [<libsym>]*
    # libgenorder_<libname>_SOURCE = src
    # libgenorder_<libname> =
    #   [<impl>:<Generator>:<topname>]*
    include(${BUILDROOT}/libgroups.cmake)
    include(${BUILDROOT}/genmappings.cmake)
    include(${BUILDROOT}/yunilibs.cmake)
    set(generators "GenRacket" "GenR7RS" "GenR6RSCommon")

    set(libsym)

    # Initialize
    foreach(e ${yunilibfiles})
        bootstrap_path_to_libsym(sym ${e})
        list(APPEND libsym ${sym})
        set(libgenorder_${sym}_source ${e})
        set(libgenorder_${sym})
        bootstrap_libsym_split(first rest ${sym})
        # Map for library group
        list(APPEND libfirst_${first} ${sym})
    endforeach()

    # Walk for each implementations
    foreach(gen ${generators})
        foreach(impl ${${gen}})
            foreach(group ${${gen}-${impl}})
                #message(STATUS "IMPL: ${impl} Group: ${group}")
                foreach(libtop ${library_group_${group}})
                    bootstrap_libmap_split(from to ${libtop})
                    #message(STATUS "From: ${from} To: ${to}")
                    foreach(sym ${libfirst_${from}})
                        bootstrap_libsym_split(first rest ${sym})
                        if(liboutput_${impl}_${to}_${rest})
                            message(STATUS 
                                "OVERRIDE!: ${impl} : ${to}_${rest} ${liboutput_${impl}_${to}_${rest}} (IGNORE: ${sym})")
                        else()
                            list(APPEND libgenorder_${sym}
                                "${impl}:${gen}:${to}")
                        endif()
                        set(liboutput_${impl}_${to}_${rest}
                            ${sym})
                    endforeach()
                endforeach()
            endforeach()
        endforeach()
    endforeach()

    # Generate libgenorder.cmake and collect libs for _yuniall
    file(WRITE ${BUILDROOT}/libgenorder.cmake
        "set(libgenorder \"${libsym}\")\n")
    set(yuniall)
    foreach(e ${yunilibfiles})
        bootstrap_libpath_strip(libname ${e})
        bootstrap_path_to_libsym(sym ${e})
        file(APPEND ${BUILDROOT}/libgenorder.cmake
            "set(libgenorder_${sym}_SOURCE \"${e}\")\n")
        file(APPEND ${BUILDROOT}/libgenorder.cmake
            "set(libgenorder_${sym}_RELSOURCE \"${libname}.sls\")\n")
        file(APPEND ${BUILDROOT}/libgenorder.cmake
            "set(libgenorder_${sym} \"${libgenorder_${sym}}\")\n")
        bootstrap_libsym_split(first bogus ${sym})
        if(${first} STREQUAL "yuni")
            # We can ignore yuniconfig here
            string(REGEX REPLACE "/" ";" libname ${libname})
            bootstrap_libname_to_sexp(libname_sexp "${libname}")
            list(APPEND yuniall "${libname_sexp}")
        endif()
    endforeach()

    # Generate _yuniall.sps
    file(WRITE ${BUILDROOT}/_yuniall.sps "(import\n")
    foreach(e ${yuniall})
        file(APPEND ${BUILDROOT}/_yuniall.sps "${e}\n")
    endforeach()
    file(APPEND ${BUILDROOT}/_yuniall.sps ")\n")
endfunction()

function(bootstrap_libname_to_basepath outvar libname)
    set(out)
    foreach(e ${libname})
        if(out)
            set(out "${out}/${e}")
        else()
            set(out "${e}")
        endif()
    endforeach()
    set(${outvar} ${out} PARENT_SCOPE)
endfunction()

function(bootstrap_libname_to_sexp outvar libname)
    set(out)
    foreach(e ${libname})
        if(NOT out)
            set(out "(${e}")
        else()
            set(out "${out} ${e}")
        endif()
    endforeach()
    set(out "${out})")
    set(${outvar} ${out} PARENT_SCOPE)
endfunction()

function(bootstrap_libmeta_to_string outvar sym sufx)
    set(str)
    foreach(e ${lib${sufx}_${sym}})
        set(str "${str}${e}\n")
    endforeach()
    set(${outvar} ${str} PARENT_SCOPE)
endfunction()

function(bootstrap_libmeta_to_string_filtered outvar impl sym sufx)
    set(str)
    foreach(e ${lib${sufx}_${sym}})
        set(str "${str}${e}\n")
    endforeach()
    set(${outvar} ${str} PARENT_SCOPE)
endfunction()

function(bootstrap_libmeta_to_string_strip outvar impl sym sufx)
    set(str)
    set(stdaux _ ... "=>" else unquote unquote-splicing)
    if(${impl} STREQUAL gauche)
        set(require_strip_keywords ON)
        set(require_strip_stdaux OFF)
    elseif(${impl} STREQUAL sagittarius)
        set(require_strip_keywords ON)
        set(require_strip_stdaux OFF)
    elseif(${impl} STREQUAL guile)
        set(require_strip_stdaux ON)
        set(require_strip_keywords OFF)
    elseif(${impl} STREQUAL chicken)
        set(require_strip_stdaux ON)
        set(require_strip_keywords OFF)
    else()
        set(require_strip_keywords OFF)
        set(require_strip_stdaux OFF)
    endif()
    if(NOT lib${sufx}_${sym})
        message(FATAL_ERROR "Zero export! ${sym}")
    endif()
    foreach(e ${lib${sufx}_${sym}})
        set(do_strip OFF)
        if(require_strip_keywords)
            string(SUBSTRING ${e} 0 1 head)
            if(${head} STREQUAL ":")
                set(do_strip ON)
            endif()
        endif()
        # Bootstrapping with Gauche may generate |...| instead of ...
        if("${e}" STREQUAL "|...|")
            set(e ...)
        endif()
        if(require_strip_stdaux)
            list(FIND stdaux ${e} idx)
            if(NOT ${idx} EQUAL -1)
                set(do_strip ON)
            endif()
        endif()
        if(NOT do_strip)
            set(str "${str}${e}\n")
        endif()
    endforeach()
    set(${outvar} ${str} PARENT_SCOPE)
endfunction()

function(bootstrap_calc_r7rs_libext outvar impl)
    # Calc ext
    if(${impl} STREQUAL gauche)
        set(ext "scm")
    elseif(${impl} STREQUAL picrin)
        set(ext "scm")
    elseif(${impl} STREQUAL chicken)
        set(ext "scm")
    elseif(${impl} STREQUAL sagittarius)
        set(ext "sagittarius.sls")
    else()
        set(ext "sld")
    endif()
    set(${outvar} ${ext} PARENT_SCOPE)
endfunction()


function(bootstrap_GenR7RS impl baselibname sls)
    bootstrap_libpath_strip(basepath ${sls})
    bootstrap_path_to_libsym(sym ${sls})
    bootstrap_libmeta_to_string_strip(exports ${impl} ${sym} exports)
    bootstrap_libmeta_to_string(imports ${sym} imports)
    bootstrap_libname_to_sexp(myname "${baselibname}")
    bootstrap_calc_r7rs_libext(libext ${impl})
    bootstrap_filter_includepath(sls ${impl})
    get_filename_component(runtimesls ${sls} NAME)
    set(outname ${STUBROOT}/${impl}/${basepath}.${libext})
    set(runtimename ${RUNTIMEROOT}/${impl}/${basepath}.${libext})
    if(${impl} STREQUAL chicken)
        set(yuniruntimename chicken)
    elseif(${impl} STREQUAL picrin)
        set(yuniruntimename picrin)
    else()
        set(yuniruntimename r7rs)
    endif()
    if(${impl} STREQUAL picrin) 
        # Picrin needs import - include - export order
        # FIXME: Is that still true?
        file(WRITE ${outname}
            "(define-library ${myname}
(import (yuni-runtime ${yuniruntimename}) ${imports})
(include \"${sls}\")
(export\n${exports}))\n")
        file(WRITE ${runtimename}
            "(define-library ${myname}
(import (yuni-runtime ${yuniruntimename}) ${imports})
(include \"${runtimesls}\")
(export\n${exports}))\n")
    else()
        # Otherwise: export - import - include (similar to R6RS)
        file(WRITE ${outname}
            "(define-library ${myname}
(export\n${exports})
(import (yuni-runtime ${yuniruntimename}) ${imports})
(include \"${sls}\"))\n")
        file(WRITE ${runtimename}
            "(define-library ${myname}
(export\n${exports})
(import (yuni-runtime ${yuniruntimename}) ${imports})
(include \"${runtimesls}\"))\n")
    endif()
endfunction()

function(bootstrap_GenR7RS_alias impl baselibname tolibname sls)
    bootstrap_libpath_strip(basepath ${sls})
    bootstrap_path_to_libsym(sym ${sls})
    bootstrap_libmeta_to_string_strip(exports ${impl} ${sym} exportsyms)
    bootstrap_libname_to_basepath(outpath "${tolibname}")
    bootstrap_libname_to_sexp(fromname "${baselibname}")
    bootstrap_libname_to_sexp(myname "${tolibname}")
    bootstrap_calc_r7rs_libext(libext ${impl})
    if(${impl} STREQUAL chicken)
        set(maybe_schemebase "(scheme base)")
    else()
        set(maybe_schemebase)
    endif()
    set(outname ${STUBROOT}/${impl}/${outpath}.${libext})
    set(runtimename ${RUNTIMEROOT}/${impl}/${outpath}.${libext})
    if(${impl} STREQUAL picrin)
        # Picrin needs import - export order
        # FIXME: Is that still true?
        file(WRITE ${outname}
            "(define-library ${myname}
(import ${fromname})
(export\n${exports}))\n")
        file(WRITE ${runtimename}
            "(define-library ${myname}
(import ${fromname})
(export\n${exports}))\n")
    else()
        # Otherwise: export - import
        file(WRITE ${outname}
            "(define-library ${myname}
(export\n${exports})
(import ${maybe_schemebase} ${fromname}))\n")
        file(WRITE ${runtimename}
            "(define-library ${myname}
(export\n${exports})
(import ${maybe_schemebase} ${fromname}))\n")
    endif()
endfunction()

function(bootstrap_GenR6RSCommon_alias impl baselibname tolibname sls)
    bootstrap_libpath_strip(basepath ${sls})
    bootstrap_path_to_libsym(sym ${sls})
    bootstrap_libmeta_to_string_strip(exports ${impl} ${sym} exportsyms)
    bootstrap_libname_to_basepath(outpath "${tolibname}")
    bootstrap_libname_to_sexp(fromname "${baselibname}")
    bootstrap_libname_to_sexp(myname "${tolibname}")
    bootstrap_calc_r7rs_libext(libext ${impl})
    set(outname ${STUBROOT}/${impl}/${outpath}.sls)
    set(runtimename ${RUNTIMEROOT}/${impl}/${outpath}.sls)
    file(WRITE ${outname}
        "(library ${myname}
(export\n${exports})
(import ${fromname}))\n")
    file(WRITE ${runtimename}
        "(library ${myname}
(export\n${exports})
(import ${fromname}))\n")
endfunction()

function(bootstrap_GenRacket_alias impl baselibname tolibname sls)
    bootstrap_path_to_libsym(sym ${sls})
    bootstrap_libmeta_to_string_strip(exports ${impl} ${sym} exportsyms)
    bootstrap_libname_to_basepath(outpath "${tolibname}")
    bootstrap_libname_to_sexp(fromname "${baselibname}")
    bootstrap_libname_to_sexp(myname "${tolibname}")
    if(${impl} STREQUAL guile)
        set(ext guile.sls)
    else()
        set(ext mzscheme.sls)
    endif()
    set(outname ${STUBROOT}/${impl}/${outpath}.${ext})
    set(runtimename ${RUNTIMEROOT}/${impl}/${outpath}.${ext})
    file(WRITE ${outname}
        "#!r6rs
(library ${myname}
    (export\n${exports})
    (import ${fromname}))")
    file(WRITE ${runtimename}
        "#!r6rs
(library ${myname}
    (export\n${exports})
    (import ${fromname}))")
endfunction()


function(bootstrap_GenRacket impl baselibname sls)
    bootstrap_path_to_libsym(sym ${sls})
    bootstrap_libmeta_to_string_strip(exports ${impl} ${sym} exports)
    bootstrap_libmeta_to_string_filtered(imports ${impl} ${sym} imports)
    bootstrap_libname_to_basepath(basepath "${baselibname}")
    bootstrap_libname_to_sexp(myname "${baselibname}")
    get_filename_component(runtimesls ${sls} NAME)
    # Calc output filename
    if(${impl} STREQUAL racket)
        set(outname ${STUBROOT}/${impl}/${basepath}.mzscheme.sls)
        set(runtimename ${RUNTIMEROOT}/${impl}/${basepath}.mzscheme.sls)
    elseif(${impl} STREQUAL guile)
        set(outname ${STUBROOT}/${impl}/${basepath}.guile.sls)
        set(runtimename ${RUNTIMEROOT}/${impl}/${basepath}.guile.sls)
    else()
        message(FATAL_ERROR "Unknown implementation: ${impl}")
    endif()
    if(${impl} STREQUAL racket)
        file(WRITE ${outname}
            "#!r6rs
(library ${myname}
    (export\n${exports})
    (import 
    (yuni-runtime ${impl})
    (only (racket) file)
    (rename (only (racket include) include) (include %%internal-paste:include))
    ${imports})
    (%%internal-paste:include (file \"${sls}\")))")
        file(WRITE ${runtimename}
            "#!r6rs
(library ${myname}
    (export\n${exports})
    (import 
    (yuni-runtime ${impl})
    (only (racket) file)
    (rename (only (racket include) include) (include %%internal-paste:include))
    ${imports})
    (%%internal-paste:include (file \"${runtimesls}\")))")
    else()
        file(WRITE ${outname}
            "#!r6rs
(library ${myname}
    (export\n${exports})
    (import 
    (yuni-runtime ${impl})
    (rename (only (guile) include) (include %%internal-paste:include))
    ${imports})
    (%%internal-paste:include \"${sls}\"))")
        file(WRITE ${runtimename}
            "#!r6rs
(library ${myname}
    (export\n${exports})
    (import 
    (yuni-runtime ${impl})
    (rename (only (guile) include) (include %%internal-paste:include))
    ${imports})
    (%%internal-paste:include \"${RUNTIMEROOT}/${impl}/${basepath}.sls\"))")
    endif()
endfunction()

function(bootstrap_generate_stublib sls orderlist)
    foreach(order ${orderlist})
        if(${order} MATCHES "([^:]*):([^:]*):([^:]*)")
            # NB: Do not add any command here..(to protect CMAKE_MATCH results)
            set(impl ${CMAKE_MATCH_1})
            set(flav ${CMAKE_MATCH_2})
            set(top ${CMAKE_MATCH_3})
            bootstrap_libpath_strip(baselibname ${sls})
            string(REGEX REPLACE "/" ";" baselibname ${baselibname})
            set(tolibname)
            foreach(e ${baselibname})
                if(NOT tolibname)
                    set(basetop ${e})
                    set(tolibname ${top})
                else()
                    list(APPEND tolibname ${e})
                endif()
            endforeach()
            if(${basetop} STREQUAL ${top})
                set(require_alias OFF)
            else()
                set(require_alias ON)
            endif()
            if(${flav} STREQUAL "GenRacket")
                bootstrap_GenRacket(${impl} "${baselibname}" ${sls})
                if(require_alias)
                    bootstrap_GenRacket_alias(${impl} 
                        "${baselibname}" "${tolibname}" ${sls})
                endif()
            elseif(${flav} STREQUAL "GenR7RS")
                bootstrap_GenR7RS(${impl} "${baselibname}" ${sls})
                if(require_alias)
                    bootstrap_GenR7RS_alias(${impl}
                        "${baselibname}" "${tolibname}" ${sls})
                endif()
            elseif(${flav} STREQUAL "GenR6RSCommon")
                if(require_alias)
                    bootstrap_GenR6RSCommon_alias(${impl}
                        "${baselibname}" "${tolibname}" ${sls})
                endif()
            else()
                message(FATAL_ERROR "Unknown flavour ${flav} for ${sls}")
            endif()
        else()
            message(FATAL_ERROR "Invalid order format: ${order}")
        endif()
    endforeach()
endfunction()

function(bootstrap_cmd_check)
    set(require_rebuild OFF)
    # Generate yunilibfiles.cmake.tmp (do not overwrite .cmake for now)
    bootstrap_gen_yunilibfiles(yunilibs.cmake.tmp)

    if(EXISTS ${BUILDROOT}/yunilibs.cmake.current)
        # Do copy-if-different against yunilibs.cmake.current
        execute_process(COMMAND
            ${CMAKE_COMMAND} -E copy_if_different
            ${BUILDROOT}/yunilibs.cmake.tmp
            ${BUILDROOT}/yunilibs.cmake.current)
        # If yunilibs.cmake.current is still older than generated,
        # the current project knows about every library files;
        # let them build and do not force-rebuild here.
        if(${BUILDROOT}/yunilibs.cmake.tmp IS_NEWER_THAN 
                ${BUILDROOT}/yunilibs.cmake.current)
            set(require_rebuild OFF)
        else()
            set(require_rebuild ON)
        endif()
    else()
        set(require_rebuild ON)
    endif()

    if(require_rebuild)
        message(FATAL_ERROR "Inconsistent builddir state. Rerun CMake.")
    else()
        # message(STATUS "Yuni library build system -- check done.")
    endif()
endfunction()

function(bootstrap_cmd_first)
    file(MAKE_DIRECTORY ${BUILDROOT})

    # Generate yunilibfiles.cmake
    bootstrap_gen_yunilibfiles(yunilibs.cmake)

    # Generate libgroups.cmake and genmappings.cmake
    bootstrap_run_scheme(CONFIG_TO_CMAKE ${YUNIROOT}/config/config.scm)

    # Integrate libgroups.cmake and others into liborder
    bootstrap_gen_librequest()
endfunction()

function(bootstrap_cmd_genlibstub)
    bootstrap_path_to_libsym(sym ${GENLIBSTUB_FILE})

    # Instantiate libmetadata
    include(${BUILDROOT}/libmeta.cmake)

    # Generate stublibs
    include(${BUILDROOT}/libgenorder.cmake)
    if(libgenorder_${sym})
        bootstrap_generate_stublib(${GENLIBSTUB_FILE} "${libgenorder_${sym}}")
    else()
        message(STATUS 
            "WARNING: Ignored ${sym} for ${e} (Not used at all?)")
    endif()
endfunction()

function(bootstrap_cmd_genlibmeta)
    bootstrap_run_scheme(LIBMETA_TO_CMAKE
        ${GENLIBMETA_IN}
        ${GENLIBMETA_OUT})
endfunction()

if(NOT BUILDROOT)
    message(FATAL_ERROR "Huh?")
endif()

if(NOT YUNIROOT)
    message(FATAL_ERROR "Huh?")
endif()

if(NOT OP)
    message(FATAL_ERROR "Huh?")
elseif(${OP} STREQUAL "BOOTSTRAP_FIRST")
    bootstrap_cmd_first()
elseif(${OP} STREQUAL "BOOTSTRAP_CHECK")
    bootstrap_cmd_check()
elseif(${OP} STREQUAL "GENLIBSTUB")
    bootstrap_cmd_genlibstub()
elseif(${OP} STREQUAL "GENLIBMETA")
    bootstrap_cmd_genlibmeta()
else()
    message(FATAL_ERROR "Unknown OP: ${OP}")
endif()

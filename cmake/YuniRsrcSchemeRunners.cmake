include(YuniDetectPlatform)

# Generate Nmosh/Sagittarius/Gauche styled loadpath list
macro(gen_loadpath0 var first)
    if(WIN32)
        set(_pathsep ";")
    else()
        set(_pathsep ":")
    endif()
    set(_out ${first})
    foreach(e ${ARGN})
        set(_out "${_out}${_pathsep}${e}")
    endforeach()
    set(${var} "${_out}")
endmacro()

macro(gen_loadpath var)
    gen_loadpath0(_tmp ${ARGN})
    set(${var} "--loadpath=${_tmp}")
endmacro()

# FIXME: Remove this
macro(gen_platformpath var plt) # FIXME: Merge with toplevel CMakeLists.txt
    if(${plt} STREQUAL "WIN64")
        set(${var} lib-stub/yunistub-win64)
    elseif(${plt} STREQUAL "WIN32")
        set(${var} lib-stub/yunistub-win32)
    elseif(${plt} STREQUAL "CYGWIN32")
        set(${var} lib-stub/yunistub-cygwin32)
    elseif(${plt} STREQUAL "CYGWIN64")
        set(${var} lib-stub/yunistub-cygwin64)
    else()
        set(${var} lib-stub/yunistub)
    endif()
endmacro()

macro(gen_libopts var opt)
    set(${var})
    foreach(e ${ARGN})
        list(APPEND ${var} "${opt}${e}")
    endforeach()
endmacro()

macro(gen_stubprefix var nam)
    set(${var}
        ${CMAKE_CURRENT_BINARY_DIR}/bootstrap/lib-stub/${nam})
endmacro()

function(calc_impl_yuniboot_commandline outvar type runtimeprefix)
    set(out)
    if("${type}" STREQUAL NMOSH)
        gen_stubprefix(stub nmosh)
        gen_loadpath(out ${stub} ${ARGN})
    elseif("${type}" STREQUAL GUILE)
        gen_stubprefix(stub guile)
        gen_libopts(out "-L;" ${stub} ${ARGN})
        set(out -l ${runtimeprefix}/lib-runtime/guile/guile-load.scm ${out})
    elseif("${type}" STREQUAL GAUCHE)
        gen_stubprefix(stub gauche)
        gen_libopts(out "-I;" ${stub} ${ARGN})
        set(out -r7 ${out} -A ${runtimeprefix})
    elseif("${type}" STREQUAL SAGITTARIUS)
        gen_stubprefix(stub sagittarius)
        gen_libopts(out "--loadpath=" ${runtimeprefix} ${stub} ${ARGN})
    elseif("${type}" STREQUAL CHIBI_SCHEME)
        gen_stubprefix(stub chibi)
        gen_libopts(out "-I;" ${runtimeprefix} ${stub} ${ARGN})
    elseif("${type}" STREQUAL RACKET)
        gen_stubprefix(stub racket)
        gen_libopts(out "++path;" ${stub} ${ARGN})
        set(out -I scheme/init -l- r6rs/run.rkt ${out})
    elseif("${type}" STREQUAL VICARE)
        gen_stubprefix(stub vicare)
        gen_libopts(out "--source-path;" ${stub} ${ARGN})
    elseif("${type}" STREQUAL CHICKEN)
        set(out -b -require-extension r7rs -s
            ${runtimeprefix}/yuniloader/yuniloader-csi.scm
            --yuniffi-stubdir ${YUNI_PLATFORM_LIBDIR})
    elseif("${type}" STREQUAL LARCENY)
        gen_stubprefix(stub larceny)
        gen_loadpath0(_loadpath ${stub} ${ARGN})
        set(out -r7r6 -path ${_loadpath} -program)
    elseif("${type}" STREQUAL CHEZ_SCHEME)
        gen_stubprefix(stub chez)
        gen_loadpath0(_loadpath ${stub} ${ARGN})
        set(out "--libdirs ${_loadpath} --program")
    elseif("${type}" STREQUAL RAPID_GAMBIT)
        gen_stubprefix(stub rapid-gambit)
        gen_libopts(out "-I;" ${stub} ${ARGN})
    elseif("${type}" STREQUAL PICRIN)
        set(out
            ${runtimeprefix}/yuniloader/yuniloader-picrin.scm
            --yuniffi-stubdir ${YUNI_PLATFORM_LIBDIR})
    elseif("${type}" STREQUAL GAMBIT)
        gen_stubprefix(stub gambit)
        gen_libopts(out "-I;" ${stub} ${ARGN})
        set(out
            ${runtimeprefix}/yuniloader/yuniloader-gsi.scm
            ${out} -MOD ${YUNI_PLATFORM_LIBDIR})
    elseif("${type}" STREQUAL MIT_SCHEME)
        gen_stubprefix(stub mit-scheme)
        gen_libopts(out "-I;" ${stub} ${ARGN})
        set(out
            --batch-mode
            --load
            ${runtimeprefix}/yuniloader/yuniloader-mit-scheme.scm
            --
            -YUNIROOT ${runtimeprefix} # MUST be on the first
            ${out} -MOD ${YUNI_PLATFORM_LIBDIR}
            -VERBOSE)
    elseif("${type}" STREQUAL IRON_SCHEME)
        gen_stubprefix(stub ironscheme)
        gen_libopts(out "-I;" ${stub} ${ARGN})
        set(out ${runtimeprefix}/yuniloader/yuniloader-ironscheme.scm
            ${out})
    else()
        message(FATAL_ERROR "Unknown scheme type ${type}")
    endif()
    set(${outvar} ${out} PARENT_SCOPE)
endfunction()

macro(gen_impl_commandline var type runtimeprefix)
    set(${var})
    if("${type}" STREQUAL NMOSH)
        gen_loadpath(${var} ${ARGN})
    else()
        if("${type}" STREQUAL GUILE)
            gen_libopts(${var} "-L;" ${ARGN})
            set(${var} -l ${runtimeprefix}/lib-runtime/guile/guile-load.scm
                ${${var}})
        elseif("${type}" STREQUAL GAUCHE)
            gen_libopts(${var} "-I;" ${ARGN})
            set(${var} -r7 ${${var}} -A ${runtimeprefix})
        elseif("${type}" STREQUAL SAGITTARIUS)
            gen_libopts(${var} "--loadpath=" ${ARGN})
        elseif("${type}" STREQUAL CHIBI_SCHEME)
            gen_libopts(${var} "-I;" ${ARGN})
        elseif("${type}" STREQUAL RACKET)
            gen_libopts(${var} "++path;" ${ARGN})
            set(${var} -I scheme/init -l- r6rs/run.rkt ${${var}})
        elseif("${type}" STREQUAL VICARE)
            gen_libopts(${var} "--source-path;" ${ARGN})
        elseif("${type}" STREQUAL CHICKEN)
            set(${var} -b -require-extension r7rs -s
                ${runtimeprefix}/yuniloader/yuniloader-csi.scm
                --yuniffi-stubdir ${YUNI_PLATFORM_LIBDIR})
        elseif("${type}" STREQUAL LARCENY)
            gen_loadpath0(_loadpath ${ARGN})
            set(${var} -r7r6 -path ${_loadpath} -program)
        elseif("${type}" STREQUAL CHEZ_SCHEME)
            gen_loadpath0(_loadpath ${ARGN})
            set(${var} "--libdirs ${_loadpath} --program")
        elseif("${type}" STREQUAL RAPID_GAMBIT)
            gen_libopts(${var} "-I;" ${ARGN})
        elseif("${type}" STREQUAL PICRIN)
            set(${var} 
                ${runtimeprefix}/yuniloader/yuniloader-picrin.scm
                --yuniffi-stubdir ${YUNI_PLATFORM_LIBDIR})
        elseif("${type}" STREQUAL GAMBIT)
            gen_libopts(${var} "-I;" ${ARGN})
            set(${var}
                ${runtimeprefix}/yuniloader/yuniloader-gsi.scm
                ${${var}} -MOD ${YUNI_PLATFORM_LIBDIR})
        elseif("${type}" STREQUAL MIT_SCHEME)
            gen_libopts(${var} "-I;" ${ARGN})
            set(${var}
                --batch-mode
                --load
                ${runtimeprefix}/yuniloader/yuniloader-mit-scheme.scm
                --
                -YUNIROOT ${runtimeprefix}
                ${${var}} -MOD ${YUNI_PLATFORM_LIBDIR})
        elseif("${type}" STREQUAL IRON_SCHEME)
            gen_libopts(${var} "-I;" ${ARGN})
        else()
            message(FATAL_ERROR "Unknown scheme type ${type}")
        endif()
    endif()
endmacro()

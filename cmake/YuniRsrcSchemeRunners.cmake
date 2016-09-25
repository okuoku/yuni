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

# Generate %XX_SEMICOLON% separated path for Larceny/Win32
macro(gen_loadpath_xx_semicolon var first)
    set(_pathsep "%XX_SEMICOLON%")
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

macro(gen_stubprefix var flav nam)
    if(${flav} STREQUAL yuniboot)
        set(${var}
            ${CMAKE_CURRENT_BINARY_DIR}/lib-stub/${nam})
    else()
        set(${var}
            ${YUNIBASE_YUNIFIED_PATH}/runtime/${nam})
    endif()
endmacro()

function(calc_impl_yuniboot_commandline outvar flav type runtimeprefix)
    set(out)
    if("${type}" STREQUAL NMOSH)
        gen_stubprefix(stub ${flav} nmosh)
        gen_loadpath(out ${stub} ${ARGN})
    elseif("${type}" STREQUAL GUILE)
        gen_stubprefix(stub ${flav} guile)
        gen_libopts(out "-L;" ${stub} ${ARGN})
        set(out -l ${runtimeprefix}/lib-runtime/guile/guile-load.scm ${out})
    elseif("${type}" STREQUAL GAUCHE)
        gen_stubprefix(stub ${flav} gauche)
        gen_libopts(out "-I;" ${stub} ${ARGN})
        set(out -r7 ${out} -A ${runtimeprefix})
    elseif("${type}" STREQUAL SAGITTARIUS)
        gen_stubprefix(stub ${flav} sagittarius)
        gen_libopts(out "--loadpath=" ${runtimeprefix} ${stub} ${ARGN})
    elseif("${type}" STREQUAL CHIBI_SCHEME)
        gen_stubprefix(stub ${flav} chibi)
        gen_libopts(out "-I;" ${runtimeprefix} ${stub} ${ARGN})
    elseif("${type}" STREQUAL RACKET)
        gen_stubprefix(stub ${flav} racket)
        gen_libopts(out "++path;" ${stub} ${ARGN})
        set(out -I scheme/init -l- r6rs/run.rkt ${out})
    elseif("${type}" STREQUAL VICARE)
        gen_stubprefix(stub ${flav} vicare)
        gen_libopts(out "--source-path;" ${stub} ${ARGN})
        #set(out --library-path ${stub} ${out})
    elseif("${type}" STREQUAL CHICKEN)
        gen_stubprefix(stub ${flav} chicken)
        gen_libopts(out "-I;" ${stub} ${ARGN})
        set(out -b -require-extension r7rs -s
            ${runtimeprefix}/yuniloader/yuniloader-csi.scm
            ${out}
            -MOD ${YUNI_PLATFORM_LIBDIR})
    elseif("${type}" STREQUAL LARCENY)
        gen_stubprefix(stub ${flav} larceny)
        if(WIN32)
            gen_loadpath_xx_semicolon(_loadpath ${stub} ${ARGN})
            # On win32, Run larceny.bin directly
            set(out -heap "${YUNI_LARCENY_ROOT}/larceny.heap"
                -r6rs -path "${_loadpath}" -program)
        else()
            gen_loadpath0(_loadpath ${stub} ${ARGN})
            set(out -r6rs -path ${_loadpath} -program)
        endif()
    elseif("${type}" STREQUAL CHEZ_SCHEME)
        gen_stubprefix(stub ${flav} chez)
        gen_loadpath0(_loadpath ${stub} ${ARGN})
        set(out "--libdirs ${_loadpath} --program")
    elseif("${type}" STREQUAL RAPID_GAMBIT)
        gen_stubprefix(stub ${flav} rapid-gambit)
        gen_libopts(out "-I;" ${stub} ${ARGN})
    elseif("${type}" STREQUAL PICRIN)
        set(out
            ${runtimeprefix}/yuniloader/yuniloader-picrin.scm
            --yuniffi-stubdir ${YUNI_PLATFORM_LIBDIR})
    elseif("${type}" STREQUAL GAMBIT)
        gen_stubprefix(stub ${flav} gambit)
        gen_libopts(out "-I;" ${stub} ${ARGN})
        set(out
            ${runtimeprefix}/yuniloader/yuniloader-gsi.scm
            ${out} -MOD ${YUNI_PLATFORM_LIBDIR})
    elseif("${type}" STREQUAL MIT_SCHEME)
        gen_stubprefix(stub ${flav} mit-scheme)
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
        gen_stubprefix(stub ${flav} ironscheme)
        gen_libopts(out "-I;" ${stub} ${ARGN})
        set(out ${runtimeprefix}/yuniloader/yuniloader-ironscheme.scm
            ${out})
    else()
        message(FATAL_ERROR "Unknown scheme type ${type}")
    endif()
    set(${outvar} ${out} PARENT_SCOPE)
endfunction()


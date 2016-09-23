add_custom_target(yuni_package)
add_custom_target(yuni_package_build)

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
# Compile: Racket, Vicare
#

precache(RACKET --compile)
# FIXME: Not working.
#precache(VICARE --build-directory ${YUNIBASE_YUNIFIED_PATH}/runtime/vicare
#    --compile-dependencies)

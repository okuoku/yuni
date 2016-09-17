add_custom_target(yuni_package)

set(allsps ${CMAKE_CURRENT_BINARY_DIR}/../bootstrap/_yuniall.sps)


macro(check_impl var nam)
    if(WIN32)
        set(_pth ${YUNIBASE_YUNIFIED_PATH}/${nam}.bat)
    else()
        set(_pth ${YUNIBASE_YUNIFIED_PATH}/${nam})
    endif()
    if(EXISTS ${_pth})
        set(YUNIBUILD_${var} ${_pth})
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

function(precache impl)
    if(YUNIBUILD_${impl})
        add_custom_target(yuni_precache_${impl} ALL
            ${YUNIBUILD_${impl}} ${ARGN} ${allsps}
            COMMENT "Precache(${impl})")
        add_dependencies(yuni_precache_${impl} yuni_bootstrap)
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

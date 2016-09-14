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

if(YUNIBUILD_GUILE)
    add_custom_target(yuni_precache_guile ALL
        ${YUNIBUILD_GUILE} ${allsps}
        )
    add_dependencies(yuni_precache_guile yuni_bootstrap)
endif()

if(YUNIBUILD_NMOSH)
    add_custom_target(yuni_precache_nmosh ALL
        ${YUNIBUILD_NMOSH} ${allsps}
        )
    add_dependencies(yuni_precache_nmosh yuni_bootstrap)
endif()


#
# Compile: Racket, Vicare
#

if(YUNIBUILD_RACKET)
    add_custom_target(yuni_precache_racket ALL
        ${YUNIBUILD_RACKET} --compile ${allsps}
        )

    add_dependencies(yuni_precache_racket yuni_bootstrap)
endif()

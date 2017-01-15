# Root module for external projects

if(__YUNI_YUNISDK_INCLUDED)
    return()
endif()

set(__YUNI_YUNISDK_INCLUDED)

set(YUNIBUILD_RUNTIME_ROOT ${YUNISDK_ROOT})
set(YUNIBUILD_RUNTIME_DEPENDS)

include(YuniDetectScheme)

yunidetectscheme_guess_bootstrap()

include(YuniDetectBootstrapScheme)

include(YuniBuild)

function(yuniapp_add_generate tgt dir impl)
    yunidetectscheme_calc_batchsuffix(sufx)
    add_custom_target(${tgt} ALL
        COMMAND
        ${YUNIBUILD_RUNTIME_ROOT}/gen-${impl}${sufx}
        ${dir})
endfunction()


#
# YuniWinSupport: Win32/Win64 support 
#

function(yuni_get_exe_abi var fn)
    file(READ ${fn} peheader OFFSET 128 LIMIT 6 HEX)
    # message(STATUS "${fn} = ${peheader}")
    if(${peheader} STREQUAL "504500006486")
        set(${var} "WIN64" PARENT_SCOPE)
    else()
        set(${var} "WIN32" PARENT_SCOPE)
    endif()
endfunction()

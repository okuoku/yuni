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

function(yuni_path_chop_drive var pth)
    get_filename_component(a ${pth} ABSOLUTE)
    file(TO_NATIVE_PATH ${a} x)
    string(SUBSTRING ${x} 2 -1 out)
    set(${var} ${out} PARENT_SCOPE)
endfunction()

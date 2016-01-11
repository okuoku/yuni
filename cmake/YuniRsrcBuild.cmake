# Yuni resource download/building support

include(CMakeParseArguments)

function(build_and_install_cmake_project projdir installdir)
    #set(_nulargs)
    #set(_uniargs)
    #set(_multiargs)
    #cmake_parse_arguments(BAIC
    #    "${_nulargs}"
    #    "${_uniargs}"
    #    "${_multiargs}")

    # Generate
    execute_process(
        COMMAND "${CMAKE_COMMAND}" 
        ${ARGN}
        "-DCMAKE_INSTALL_PREFIX=${installdir}"
        ${projdir}
        WORKING_DIRECTORY ${projdir}
        RESULT_VARIABLE rr)
    if(rr)
        message(FATAL_ERROR "Cannot configure ${projdir}")
    endif()

    # Build(Release)
    execute_process(
        COMMAND "${CMAKE_COMMAND}"
        --build ${projdir} --config Release
        WORKING_DIRECTORY ${projdir}
        RESULT_VARIABLE rr)
    if(rr)
        message(FATAL_ERROR "Error in build phase ${projdir}")
    endif()

    # Install(Release)
    execute_process(
        COMMAND "${CMAKE_COMMAND}"
        --build ${projdir} --config Release
        --target install
        WORKING_DIRECTORY ${projdir}
        RESULT_VARIABLE rr)
    if(rr)
        message(FATAL_ERROR "Error in install phase ${projdir}")
    endif()
endfunction()

function(download_and_extract uri destdir)
    file(MAKE_DIRECTORY ${destdir})
    if(uri MATCHES "\\.tar\\.gz$")
        set(_extract_command zxvf)
        set(_archivename archive.tar.gz)
    elseif(uri MATCHES "\\.tar\\.bz2$")
        set(_extract_command jxvf)
        set(_archivename archive.tar.bz2)
    elseif(uri MATCHES "\\.zip$")
        set(_extract_command zxvf)
        set(_archivename archive.zip)
    else()
        message(FATAL_ERROR "Cannot detect archive format ${uri}")
    endif()
    set(_outfile ${destdir}/${_archivename})
    file(DOWNLOAD
        ${uri}
        ${_outfile}
        TLS_VERIFY OFF)
    execute_process(
        COMMAND "${CMAKE_COMMAND}" -E tar 
        ${_extract_command} ${_archivename}
        WORKING_DIRECTORY ${destdir}
        RESULT_VARIABLE rr)
    if(rr)
        message(FATAL_ERROR "Cannot extract archive ${rr}")
    endif()
endfunction()


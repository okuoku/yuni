if(YUNI_GOSH AND YUNI_GAUCHE_PACKAGE)
    if(CYGWIN OR WIN32)
        set(module_sufx "dll")
    else()
        set(module_sufx "so")
    endif()
    set(output_name ${CMAKE_CURRENT_BINARY_DIR}/yuniffi.${module_sufx})
    set(callgenstub ${CMAKE_CURRENT_BINARY_DIR}/call-genstub.scm)
    set(srcs
        ${CMAKE_CURRENT_LIST_DIR}/yuniffi.c)
    set(stubfile
        ${CMAKE_CURRENT_LIST_DIR}/yuniffilib.stub)
    include_directories(${CMAKE_CURRENT_LIST_DIR}/../include
        ${CMAKE_CURRENT_LIST_DIR})

    file(WRITE ${callgenstub}
        "(use gauche.cgen.stub)\n(cgen-genstub \"${stubfile}\")\n")
    add_custom_command(OUTPUT yuniffilib.c
        COMMAND ${YUNI_GOSH} ${callgenstub}
        DEPENDS 
        ${stubfile}
        ${callgenstub})
    add_custom_command(OUTPUT ${output_name}
        COMMAND ${YUNI_GAUCHE_PACKAGE} compile
        --cppflags=-I${CMAKE_CURRENT_LIST_DIR}/../include
        # HACK!
        --cflags=-I${CMAKE_CURRENT_LIST_DIR}
        yuniffi
        ${srcs}
        yuniffilib.c
        DEPENDS
        ${srcs}
        yuniffilib.c
        IMPLICIT_DEPENDS ${srcs})
    add_custom_target(gauche-yuniffi ALL DEPENDS ${output_name})
    yuni_install_native_programs(${output_name})
    yuni_install_native_files(
        ${CMAKE_CURRENT_LIST_DIR}/yuniffi.scm)
endif()

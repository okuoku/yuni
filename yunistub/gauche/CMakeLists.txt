if(YUNI_GOSH AND YUNI_GAUCHE_PACKAGE)
    if(MSVC)
        # FIXME: Ignore MSVC based build for now
        return()
    endif()
    if(CYGWIN OR WIN32)
        set(module_sufx "dll")
    else()
        set(module_sufx "so")
    endif()
    set(output_name ${CMAKE_CURRENT_BINARY_DIR}/yuniffi.${module_sufx})
    set(srcs
        ${CMAKE_CURRENT_LIST_DIR}/yuniffi.c)
    set(stubfile
        ${CMAKE_CURRENT_LIST_DIR}/yuniffilib.stub)
    set(callgenstub
        ${CMAKE_CURRENT_BINARY_DIR}/callgenstub.scm)
    include_directories(${CMAKE_CURRENT_LIST_DIR}/../include
        ${CMAKE_CURRENT_LIST_DIR})
    configure_file(${CMAKE_CURRENT_LIST_DIR}/callgenstub.scm.in
        ${callgenstub} @ONLY)
    add_custom_command(OUTPUT yuniffilib.c
        COMMAND ${YUNI_GOSH}
        ${callgenstub}
        DEPENDS 
        callgenstub.scm.in
        ${stubfile})
    add_custom_command(OUTPUT ${output_name}
        COMMAND ${YUNI_GAUCHE_PACKAGE} compile
        --cppflags=-I${CMAKE_CURRENT_LIST_DIR}/../include
        # HACK! (to add 2 include paths)
        --cflags=-I${CMAKE_CURRENT_LIST_DIR}
        --cc=${CMAKE_C_COMPILER}
        yuniffi
        ${srcs}
        yuniffilib.c
        DEPENDS
        ${srcs}
        yuniffilib.c
        IMPLICIT_DEPENDS C ${srcs})
    add_custom_target(gauche-yuniffi ALL DEPENDS ${output_name})
    yuni_runtime_deploy(
        PROGRAMS ${output_name})
    yuni_runtime_deploy(
        FILES ${CMAKE_CURRENT_LIST_DIR}/yuniffi.scm)
endif()

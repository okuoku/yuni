set(srcs
    ${CMAKE_CURRENT_LIST_DIR}/yuniffi-chicken.scm)
set(c_srcs
    ${CMAKE_CURRENT_LIST_DIR}/../common/bootstrap.inc.c)

if(YUNI_CHICKEN AND YUNI_CHICKEN_CSC)
    if(CYGWIN OR WIN32)
        set(module_sufx "dll")
    else()
        set(module_sufx "so")
    endif()
    set(output_name ${CMAKE_CURRENT_BINARY_DIR}/yuniffi-chicken.${module_sufx})
    include_directories(${CMAKE_CURRENT_LIST_DIR}/../include)
    add_custom_command(OUTPUT ${output_name}
        COMMAND ${YUNI_CHICKEN_CSC}
        -dynamic
        -o ${output_name}
        -I${CMAKE_CURRENT_LIST_DIR}/../include
        ${srcs}
        DEPENDS
        ${srcs}
        IMPLICIT_DEPENDS C
        ${c_srcs})
    add_custom_target(yuniffi-chicken ALL DEPENDS ${output_name})
    yuni_runtime_deploy(PROGRAMS ${output_name})
endif()

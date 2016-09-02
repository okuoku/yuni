set(srcs
    ${CMAKE_CURRENT_LIST_DIR}/yuniffi-gambit.scm)
set(c_srcs
    ${CMAKE_CURRENT_LIST_DIR}/../common/bootstrap.inc.c)

set(gambit_available)
if(WIN32)
    if(YUNI_GSC AND YUNI_GCC)
        set(gambit_available ON)
    endif()
else()
    if(YUNI_GSC)
        set(gambit_available ON)
    endif()
endif()

if(gambit_available)
    set(output_name ${CMAKE_CURRENT_BINARY_DIR}/yuniffi-gambit.o1)
    include_directories(${CMAKE_CURRENT_LIST_DIR}/../include)
    add_custom_command(OUTPUT ${output_name}
        COMMAND ${YUNI_GSC}
        -dynamic
        -cc-options -I${CMAKE_CURRENT_LIST_DIR}/../include
        -o ${output_name}
        ${srcs}
        DEPENDS
        ${srcs}
        IMPLICIT_DEPENDS C
        ${c_srcs})
    add_custom_target(yuniffi-gambit ALL DEPENDS ${output_name})
    yuni_install_native_programs(${output_name})
endif()

# yunistub for chibi-scheme

include_directories(${CMAKE_CURRENT_LIST_DIR}
    ${CMAKE_CURRENT_LIST_DIR}/../include)

if(YUNI_CHIBI_FFI)
    set(stub ${CMAKE_CURRENT_BINARY_DIR}/yuniffi.stub.c)
    set(stubfile "${CMAKE_CURRENT_LIST_DIR}/yuniffi.stub.scm")

    # Generate stub source
    add_custom_command(OUTPUT ${stub}
        COMMAND ${YUNI_CHIBI_FFI} ${stubfile} ${stub}
        DEPENDS ${stubfile})
    add_library(chibi-yuniffi MODULE
        yuniffi_stub.c
        ${stub})
    set_target_properties(chibi-yuniffi
        PROPERTIES
        PREFIX ""
        LIBRARY_OUTPUT_NAME chibi-yuniffi)
    target_link_libraries(chibi-yuniffi ${YUNI_CHIBI_LIB}) 
    yuni_install_native_targets(chibi-yuniffi)
endif()

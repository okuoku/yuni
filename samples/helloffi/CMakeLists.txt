cmake_minimum_required(VERSION 2.8)
project(samples-helloffi)

if(NOT YUNISDK_ROOT)
    message(FATAL_ERROR
        "YUNISDK_ROOT is not set (/path/to/yunified)")
endif()

list(APPEND CMAKE_MODULE_PATH
    ${YUNISDK_ROOT}/cmake/yuni)


include(YuniSdk)

# YuniFFI stub build

set(yunilibdir ${CMAKE_CURRENT_BINARY_DIR}/_yunistub)
set(yunilibroot ${yunilibdir}/yunistub)
 
file(MAKE_DIRECTORY ${yunilibroot})

yunibuild_add_yunistub(stubbuild
    ${CMAKE_CURRENT_LIST_DIR}/helloffi_stub.scm
    # Output dirs
    ${yunilibroot}
    ${CMAKE_CURRENT_BINARY_DIR}
    # Stub C source name
    helloffi.stub.c)

include_directories(. ${YUNISDK_ROOT}/include)

add_library(yunistub_helloffi_stub MODULE
    ${CMAKE_CURRENT_BINARY_DIR}/helloffi.stub.c
    ${CMAKE_CURRENT_LIST_DIR}/helloffi.c)

# Deploy the library to library dir
add_custom_command(TARGET yunistub_helloffi_stub
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
    $<TARGET_FILE:yunistub_helloffi_stub> 
    ${CMAKE_CURRENT_BINARY_DIR}/_yunistub/yunistub)

foreach(e racket gosh chibi-scheme sagittarius)
    yuniapp_add_generate(gen-${e}
        ${CMAKE_CURRENT_LIST_DIR}
        ${e})
endforeach()


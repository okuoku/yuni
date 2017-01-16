list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/cmake)

set(SDL2_BUILDING_LIBRARY TRUE)
find_package(SDL2)

if(SDL2_FOUND)
    include_directories(${SDL2_INCLUDE_DIR})
    gen_yuniffi_stub(SDL2-yunistub
        ${CMAKE_CURRENT_LIST_DIR}/SDL2.scm
        SDL2.stub.c)
    add_library(yunistub_SDL2 MODULE
        ${CMAKE_CURRENT_BINARY_DIR}/SDL2.stub.c
        )
    add_dependencies(yunistub_SDL2 SDL2-yunistub)
    target_link_libraries(yunistub_SDL2 
        ${SDL2_LIBRARY})
endif()

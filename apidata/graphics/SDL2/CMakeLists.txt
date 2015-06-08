list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/cmake)

set(SDL2_BUILDING_LIBRARY TRUE)
find_package(SDL2)

if(SDL2_FOUND)
    include_directories(${SDL2_INCLUDE_DIR})
    add_library(yunistub_SDL2 MODULE
        ${YUNISTUB_STUBSOURCE_PREFIX}/graphics/SDL2/SDL2.stub.c
        )
    target_link_libraries(yunistub_SDL2 
        ${SDL2_LIBRARY})
    yuni_install_native_targets(yunistub_SDL2)
endif()

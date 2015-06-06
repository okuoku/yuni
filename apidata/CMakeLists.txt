cmake_minimum_required(VERSION 2.8)
set(CMAKE_LEGACY_CYGWIN_WIN32 0)
project(yunistubs)

include_directories(${CMAKE_CURRENT_LIST_DIR}/../yunistub/include)

set(YUNISTUB_STUBSOURCE_PREFIX ${CMAKE_CURRENT_LIST_DIR}/../lib-stub/apistubs/yunistub)

add_subdirectory(graphics/SDL2 SDL2)

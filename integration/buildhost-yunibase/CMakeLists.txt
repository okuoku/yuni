# Prelude for bootstrappin' on standard yunibuild docker images
cmake_minimum_required(VERSION 2.8)

set(YUNI_WITH_YUNIBASE /build CACHE "" PATH FORCE)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/../../cmake)

# Use nmosh on yunibase

set(YUNI_BOOTSTRAP_STAGE ${CMAKE_CURRENT_BINARY_DIR})
set(YUNI_BOOTSTRAP_NMOSH /build/stable/nmosh/bin/nmosh)
set(YUNI_BOOTSTRAP_ROOT ${CMAKE_CURRENT_LIST_DIR}/../..)

include(../yuni_tree_bootstrap.cmake)

add_subdirectory(../.. yuni)

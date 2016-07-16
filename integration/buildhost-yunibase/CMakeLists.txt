# Prelude for bootstrappin' on standard yunibuild docker images
cmake_minimum_required(VERSION 2.8.4)
enable_testing()

if(NOT YUNI_WITH_YUNIBASE)
    set(YUNI_WITH_YUNIBASE /build CACHE "" PATH FORCE)
endif()

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/../../cmake)


if(WIN32)
    # Use nmosh on yunibase
    set(YUNI_BOOTSTRAP_STAGE ${CMAKE_CURRENT_BINARY_DIR})
    set(YUNI_BOOTSTRAP_NMOSH ${YUNI_WITH_YUNIBASE}/stable/nmosh/bin/nmosh)
    set(YUNI_BOOTSTRAP_ROOT ${CMAKE_CURRENT_LIST_DIR}/../..)
else()
    # Use chibi-scheme on yunibase
    set(YUNI_BOOTSTRAP_STAGE ${CMAKE_CURRENT_BINARY_DIR})
    set(YUNI_BOOTSTRAP_NMOSH ${YUNI_WITH_YUNIBASE}/stable/nmosh/bin/nmosh)
    set(YUNI_BOOTSTRAP_CHIBI_SCHEME ${YUNI_WITH_YUNIBASE}/current/chibi-scheme/bin/chibi-scheme)
    set(YUNI_BOOTSTRAP_ROOT ${CMAKE_CURRENT_LIST_DIR}/../..)

    # Update LD_LIBRARY_PATH until configure
    set(ENV{LD_LIBRARY_PATH}
        "${YUNI_WITH_YUNIBASE}/current/chibi-scheme/lib:$ENV{LD_LIBRARY_PATH}")
endif()

include(../yuni_tree_bootstrap.cmake)

add_subdirectory(../.. yuni)

project(yuni)
cmake_minimum_required(VERSION 3.0)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/../../cmake)

include(YuniDetectPlatform)
include(YuniDetectScheme)

include(YuniRsrcBuild)
include(YuniRsrcSchemeHosts)

set(_dest ${CMAKE_CURRENT_BINARY_DIR})

download_and_extract(
    ${YUNI_NMOSH_RELEASE_URI}
    ${_dest}/nmosh-current)

download_and_extract(
    ${GMP_RELEASE_URI}
    ${_dest}/gmp-current)

build_and_install_cmake_project(
    "${CMAKE_CURRENT_BINARY_DIR}/nmosh-current/mosh-0.2.7"
    "${CMAKE_CURRENT_BINARY_DIR}/stage-bootstrap"
    -G "Visual Studio 14 2015 Win64"
    -DMOSH_PREFIXLESS=OFF
    -DMOSH_MP_BACKEND=MINI_GMP
    -DMOSH_MP_MINI_GMP_DIR=${CMAKE_CURRENT_BINARY_DIR}/gmp-current/gmp-6.1.0
    )

set(YUNI_BOOTSTRAP_STAGE ${_dest}/stage-bootstrap)
set(YUNI_BOOTSTRAP_NMOSH ${YUNI_BOOTSTRAP_STAGE}/bin/nmosh.exe)
set(YUNI_BOOTSTRAP_ROOT ${CMAKE_CURRENT_LIST_DIR}/../..)

include(../yuni_tree_bootstrap.cmake)


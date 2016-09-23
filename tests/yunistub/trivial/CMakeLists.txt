# Prefer CXX version

include_directories(${CMAKE_CURRENT_LIST_DIR})

add_yuniffi_stub(stub_yunistub_testing_trivial
    ${CMAKE_CURRENT_LIST_DIR}/testing.scm
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
    testing.stub.cpp testing.stub.c)

add_library(yunistub_testing_trivial MODULE
    ${CMAKE_CURRENT_BINARY_DIR}/testing.stub.cpp
    testfunc.c
    testing.h
    testinterface.h)

add_library(yunistub_testing_trivial_plainc MODULE
    ${CMAKE_CURRENT_BINARY_DIR}/testing.stub.c
    testfunc.c
    testing.h
    testinterface.h)

yuni_install_native_targets(yunistub_testing_trivial)

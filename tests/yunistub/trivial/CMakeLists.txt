# Prefer CXX version

include_directories(${CMAKE_CURRENT_LIST_DIR})

add_library(yunistub_testing_trivial MODULE
    ${YUNISTUB_STUBSOURCE_PREFIX}/testing/trivial/testing.stub.cpp
    testfunc.c
    testing.h
    testinterface.h)

add_library(yunistub_testing_trivial_plainc MODULE
    ${YUNISTUB_STUBSOURCE_PREFIX}/testing/trivial/testing.stub.c
    testfunc.c
    testing.h
    testinterface.h)

yuni_install_native_targets(yunistub_testing_trivial)

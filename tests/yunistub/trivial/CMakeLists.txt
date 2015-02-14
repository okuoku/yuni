cmake_minimum_required(VERSION 2.8)
project(yunistub-trivial)

include_directories(${CMAKE_CURRENT_LIST_DIR}/../../../yunistub/include)

add_library(checkhand_c MODULE
    testhand.c
    dummy.cpp
    testfunc.c
    testing.h
    testinterface.h)

add_library(checkhand_cxx MODULE
    testhand.cpp
    dummy.cpp
    testfunc.c
    testing.h
    testinterface.h)

add_library(check MODULE
    dummy.cpp
    testfunc.c
    testing.h
    testinterface.h)



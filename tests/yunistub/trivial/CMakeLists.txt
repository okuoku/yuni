cmake_minimum_required(VERSION 2.8)
project(yunistub-trivial)

add_library(check MODULE
    dummy.cpp
    testfunc.c
    testing.h
    testinterface.h)


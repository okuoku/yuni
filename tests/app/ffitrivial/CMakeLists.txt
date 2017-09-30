set(stubtop ${CMAKE_CURRENT_LIST_DIR}/stub)
set(inc ${CMAKE_CURRENT_LIST_DIR}/../../../yunistub/include)

set(yunilibdir ${CMAKE_CURRENT_BINARY_DIR}/_yunistub)
set(yunilibroot ${yunilibdir}/yunistub)

file(MAKE_DIRECTORY ${yunilibroot})

include_directories(
    ${inc}
    ${stubtop})

yunibuild_add_yunistub(
    test_ffitrivial_trivial
    ${stubtop}/testing_trivial.scm
    # Output dirs
    ${yunilibroot}
    ${CMAKE_CURRENT_BINARY_DIR}
    # Stub C/C++ source names (specified in stublib)
    testing.stub.c
    testing.stub.cpp)

# yunibuild_add_yunistub(
#     test_ffitrivial_subtest
#     ${stubtop}/unnamed.scm
#     # Output dirs
#     ${yunilibroot}
#     ${CMAKE_CURRENT_BINARY_DIR}
#     # Stub C/C++ source names (specified in stublib)
#     unnamed.stub.c)

add_library(yunistub_testing_trivial MODULE
    ${CMAKE_CURRENT_BINARY_DIR}/testing.stub.cpp
    ${stubtop}/testfunc.c
    ${stubtop}/testing.h
    ${stubtop}/testinterface.h)

add_library(yunistub_testing_trivial_plainc MODULE
    ${CMAKE_CURRENT_BINARY_DIR}/testing.stub.c
    ${stubtop}/testfunc.c
    ${stubtop}/testing.h
    ${stubtop}/testinterface.h)

# Since we have duplicated dependencies on generated files,
# serialize build here
add_dependencies(yunistub_testing_trivial_plainc
    test_ffitrivial_trivial)

add_dependencies(yunistub_testing_trivial
    yunistub_testing_trivial_plainc
    test_ffitrivial_trivial)

# add_library(yunistub_testing_unnamed MODULE
#     ${CMAKE_CURRENT_BINARY_DIR}/unnamed.stub.c)

# Deploy the module to library dir
add_custom_command(TARGET yunistub_testing_trivial 
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
    $<TARGET_FILE:yunistub_testing_trivial>
    ${yunilibdir})


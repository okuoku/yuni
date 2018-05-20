#
# s7yuni build file
#
#  FIXME: Support Win32
#  FIXME: Implement r7rs stuffs without cload 


cmake_minimum_required(VERSION 2.8.11)
project(s7yuni)

if(NOT S7YUNI_S7_ROOT)
    set(S7YUNI_S7_ROOT ${CMAKE_CURRENT_LIST_DIR})
endif()

set(S7YUNI_DEFAULT_LOAD_PATH ${CMAKE_INSTALL_PREFIX}/share/s7yuni)

include_directories(
    ${CMAKE_CURRENT_BINARY_DIR}
    ${S7YUNI_S7_ROOT})

add_definitions(
    -DS7YUNI_DEFAULT_LOAD_PATH=\"${S7YUNI_DEFAULT_LOAD_PATH}\")

configure_file(mus-config.h.in ${CMAKE_CURRENT_BINARY_DIR}/mus-config.h
    @ONLY)

add_executable(s7yuni
    s7yuni.c
    ${S7YUNI_S7_ROOT}/s7.c
    )

if(CYGWIN)
    # FIXME: Generate implib here
    set_target_properties(
        s7yuni
        PROPERTIES LINK_FLAGS -Wl,--export-all-symbols)
else(UNIX)
    # Assume ELF platforms
    set_target_properties(
        s7yuni
        PROPERTIES LINK_FLAGS -Wl,--export-dynamic)
endif()

if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    target_link_libraries(s7yuni
        m dl)
endif()

set(s7yuni_libs
    s7yuniboot.scm)

set(libs_src
    cload.scm
    gtkex.scm
    json.scm
    libc.scm
    libdl.scm
    libgdbm.scm
    libgsl.scm
    libm.scm
    libutf8proc.scm
    lint.scm
    mockery.scm
    profile.scm
    r7rs.scm
    repl.scm
    # snd-lint.scm # Is snd specific
    stuff.scm
    write.scm)

foreach(e ${libs_src})
    list(APPEND libs ${S7YUNI_S7_ROOT}/${e})
endforeach()

install(TARGETS
    s7yuni DESTINATION bin)
install(FILES
    ${libs} ${s7yuni_libs}
    DESTINATION share/s7yuni)

add_subdirectory(chibi-scheme)
add_subdirectory(gauche)
add_subdirectory(chicken)
add_subdirectory(gambit)
# Currently, picrin cannot be built here.

# Headers
install(DIRECTORY include
    DESTINATION ${YUNISDK_ROOT})

function(installdep f)
    configure_file(${f}
        ${YUNIBASE_YUNIFIED_PATH}/loader/${f}
        COPYONLY)
endfunction()

set(deps
    generate.sps
    yuniffistub.sps
    guile-load.scm)

foreach(e ${deps})
    installdep(${e})
endforeach()


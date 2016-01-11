macro(rsrc_uri nam arg)
    set(${nam} ${arg})
endmacro()

rsrc_uri(YUNI_NMOSH_RELEASE_URI
    "http://storage.osdev.info/pub/mosh/mosh-current.tar.gz")

rsrc_uri(GMP_RELEASE_URI
    "http://storage.osdev.info/pub/proj/yuni/gmp-6.1.0.tar.bz2")


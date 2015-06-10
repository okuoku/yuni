UNAMES=`uname -s`
UNAMEM=`uname -m`

case "$UNAMEM$UNAMES" in
    i?86CYGWIN_*) 
        YUNIMOD="lib-stub/yunistub-cygwin32"
        ;;
    x86_64CYGWIN_*) 
        YUNIMOD="lib-stub/yunistub-cygwin64"
        ;;
    *) YUNIMOD="lib-stub/yunistub" ;;
esac

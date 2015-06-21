#!/bin/sh
. run/_ostype.sh
exec chibi-scheme -I$YUNIMOD -I lib-stub/chibi -I lib-runtime/r7rs -I lib-runtime/chibi $*

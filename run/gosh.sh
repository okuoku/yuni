#!/bin/sh
. run/_ostype.sh
exec gosh -r7 -I $YUNIMOD -I lib-runtime/gauche -I lib-runtime/r7rs -I lib-stub/gauche -A . $*

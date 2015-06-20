#!/bin/sh
. run/_ostype.sh
exec nmosh --loadpath=$YUNIMOD:lib-runtime/nmosh:lib-r6rs:lib-stub/r6rs-common:lib:lib-compat:lib-compat/yuni-srfi:lib-stub/nmosh $*

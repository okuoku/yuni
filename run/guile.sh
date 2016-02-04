#!/bin/sh
. run/_ostype.sh
exec guile -l lib-runtime/guile/guile-load.scm -L lib-runtime/guile -L lib-stub/guile -L $YUNIMOD -L lib-r6rs -L lib-stub/r6rs-common -L lib-stub/gen -L lib -L lib-compat -L lib-compat/yuni-srfi $*

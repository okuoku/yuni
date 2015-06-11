#!/bin/sh
. run/_ostype.sh
exec guile -l lib-runtime/guile/guile-load.scm -L $YUNIMOD -L lib-r6rs -L lib-stub/guile -L lib-stub/r6rs-common -L lib -L lib-compat -L lib-compat/yuni-srfi $*

#!/bin/sh
exec guile -l lib-runtime/guile/guile-load.scm -L lib-r6rs -L lib-stub/guile -L lib-stub/r6rs-common -L lib -L lib-compat -L lib-compat/yuni-srfi $*

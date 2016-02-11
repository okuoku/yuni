#!/bin/sh

#exec java -Dkawa.import.path="lib-stub/kawa/*.sld" -classpath ./kawa-2.0.jar kawa.repl --r7rs --script lib-runtime/kawa/yuniloader.scm -- $*
mydir=`pwd`
exec java -Dkawa.import.path="$mydir/lib-stub/kawa/*.sld" kawa.repl --r7rs --full-tailcalls --no-inline --script lib-runtime/kawa/yuniloader.scm -- $*

# FIXME: It seems we cannot use `:` for path separator in Win32...

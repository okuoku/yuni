#!/bin/sh
. run/_ostype.sh
exec vicare --source-path $YUNIMOD --source-path lib-r6rs --source-path lib-stub/vicare --source-path lib-stub/r6rs-common --source-path lib-compat --source-path lib --source-path lib-stub/gen $*

# NB: Vicare uses --library-path for FASL path, 
#     --source-path for .sls path.

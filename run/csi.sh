#!/bin/sh
. run/_ostype.sh
exec csi -b -require-extension r7rs -s lib-runtime/r7rs/yuniloader-csi.scm --yuniffi-stubdir $YUNIMOD $*


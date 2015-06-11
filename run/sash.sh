#!/bin/sh
source run/_ostype.sh
exec sash --loadpath=$YUNIMOD --loadpath=lib-runtime/r7rs --loadpath=lib-stub/r6rs-common --loadpath=lib-stub/sagittarius --loadpath=lib-compat $*

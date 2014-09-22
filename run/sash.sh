#!/bin/sh
exec sash --loadpath=lib-runtime/r7rs --loadpath=lib-stub/r6rs-common --loadpath=lib-stub/sagittarius --loadpath=lib-compat $*

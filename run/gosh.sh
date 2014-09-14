#!/bin/sh
exec gosh -r7 -I lib-runtime/gauche -I lib-runtime/r7rs -I lib-stub/gauche -A . $*

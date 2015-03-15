#!/bin/sh
exec chibi-scheme -I lib-stub/chibi -I lib-runtime/r7rs -I lib-runtime/chibi $*

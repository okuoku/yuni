#!/bin/sh
exec nmosh --loadpath=lib-r6rs:lib-stub/r6rs-common:lib:lib-compat:lib-compat/yuni-srfi:lib-stub/nmosh $*

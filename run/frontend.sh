#!/bin/sh
nmosh --loadpath=lib-stub/r6rs-common:lib:lib-compat:lib-frontend --guru-mode --verbose scripts/frontend.sps $*

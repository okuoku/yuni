#!/bin/sh

nmosh --loadpath=lib-bootstrap:lib:lib-compat scripts/build-nmosh.sps
exec run/nmosh.sh scripts/build-apistubs-nmosh.sps

#!/bin/sh

nmosh --loadpath=scripts/lib-bootstrap:lib:lib-compat scripts/build-nmosh.sps && run/nmosh.sh scripts/build-apistubs-nmosh.sps && nmosh --loadpath=scripts/lib-bootstrap:lib:lib-compat scripts/build-nmosh.sps 

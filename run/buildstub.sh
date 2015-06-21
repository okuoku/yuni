#!/bin/sh

nmosh --loadpath=lib-bootstrap:lib:lib-compat scripts/build-nmosh.sps && run/nmosh.sh scripts/build-apistubs-nmosh.sps && nmosh --loadpath=lib-bootstrap:lib:lib-compat scripts/build-nmosh.sps 

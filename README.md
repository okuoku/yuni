yuni
----

R7RS/R6RS Scheme portable library(Under construction)

## (ultimate) Goals

These are not implemented yet though.

* Common application runner to make Scheme implementations as interchangable script engine
* Common convenient syntaxes: async, let1, ...
* Multithreading
* Portable in-system debugger
* yuniFFI: FFI wrapper for dynamic/static bindings
* Provide 3rd-party tests 

## Non-goals

YUNI IS NOT FOR PERFORMANCE BENCHMARKING. Making performance optimal library for each Scheme implementation is obviously not our goal. 

* Performance (Portability over performance)

## Licensing

(TBD. Each source should include its own license terms. We will provide combined license document later for binary distributions.)

Build
-----

Every implementation requires `lib-stub` which will contain library import stubs.
To generate library import stub, install nmosh from http://storage.osdev.info/pub/mosh/mosh-current.tar.gz and run:

 `run/buildstub.sh`


yuni
----

R7RS/R6RS Scheme portable library(Under construction)

## (ultimate) Goals

These are not implemented yet though.

* Common convenient syntaxes: async, let1, ...
* Multithreading
* Portable in-system debugger
* FFI wrapper for dynamic/static bindings
  * Unified C Interface Description(UCID) as common API description format
* Provide 3rd-party tests 

## Non-goals

YUNI IS NOT FOR PERFORMANCE BENCHMARKING. Making performance optimal library for each Scheme implementation is obviously not our goal. 

* Performance (Portability over performance)

## See also

* doc/PortingNotes - for implementation specific features

## Licensing

(TBD. Each source should include its own license terms. We will provide combined license document later for binary distributions.)

Library rules
-------------

## Using base implementation

When you import something(syntax and procedure) from base implementation, it should be located on: 

* (yuni scheme)
* (yuni compat ...)

Do not import (rnrs) or (scheme base) directly in any other libraries.

## Importing other FOSS Scheme libraries

(TBD. In-a-nutshell, avoid LGPL/GPL/CDDL here. Standard SRFI license is fine in most cases.)

Library configuration
---------------------

TBD

External libraries
------------------


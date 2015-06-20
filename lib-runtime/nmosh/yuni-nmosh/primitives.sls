(library (yuni-nmosh primitives)
         (export 
           sys-get-bytevector
           sys-open-bytevector-output-port
           prefix-list
           bytevector-pointer)
         (import (primitives 
                   sys-get-bytevector sys-open-bytevector-output-port
                   prefix-list bytevector-pointer)))

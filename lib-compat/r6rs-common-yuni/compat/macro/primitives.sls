;; Bridge library to export identifiers on multiple meta-levels
(library (r6rs-common-yuni compat macro primitives)
         (export syntax-inject)
         (import (for (yuni compat macro primitives0) run expand)))

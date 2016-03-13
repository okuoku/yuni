(library (r7c-system expander)
         (export $define/primitive
                 $bind-definition
                 $bind-variable
                 $extend-env
                 $inject
                 $quote
                 $alias)
         (import (r7c-expander-interface)))

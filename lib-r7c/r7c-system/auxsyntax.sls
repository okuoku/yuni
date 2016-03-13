(library (r7c-system auxsyntax)
         (export _ ...  => 
                 unquote unquote-splicing
                 else)
         (import (r7c-expander-interface))

($define-aux-syntax _)
($define-aux-syntax ...)
($define-aux-syntax =>)
($define-aux-syntax unquote)
($define-aux-syntax unquote-splicing)
($define-aux-syntax else)

)

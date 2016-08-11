(define-library (yuniffi-config)
                (export %%yuniffi-module-prefix-set!
                        %%yuniffi-module-prefix)
                (import (scheme base))
                (begin
                  (define module-prefix #f)
                  (define (%%yuniffi-module-prefix)
                    module-prefix)
                  (define (%%yuniffi-module-prefix-set! prefix)
                    (set! module-prefix prefix))))

(library (mit-scheme-compat-scheme time)
         (export
           current-jiffy current-second jiffies-per-second)
         (import)

(define-primitive-names/yunifake
  current-jiffy current-second jiffies-per-second)
)

(library (yuni eval)
         (export eval environment)
         (import 
           (yuni scheme) ;; FIXME: Chicken WAR
           (yuni compat eval)))

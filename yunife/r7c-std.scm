((r7c heap core)
 eq?)

((r7c heap eqv)
 eqv?)

((r7c heap equal)
 equal?)

((r7c heap pair)
 pair?
 null?
 cons
 car
 cdr
 set-car!
 set-cdr!)

((r7c heap vector)
 list->vector
 vector->list)

((r7c heap list)
 list
 append)

((r7c heap listloop)
 memv)

((r7c core values)
 values
 call-with-values)

((r7c core callcc)
 call/cc
 call-with-current-continuation)

((r7c core boolean)
 not)

((r7c core dynamic-wind)
 dynamic-wind)

((r7c core exception)
 with-exception-handler
 raise
 raise-continuable)

((r7c core apply)
 apply)

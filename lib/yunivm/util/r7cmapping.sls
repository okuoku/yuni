(library (yunivm util r7cmapping)
         (export
           r7cmapping/coreproc)
         (import (yuni scheme))

(define r7cmapping/coreproc
  '(;; Procedures required for R7RS standard syntaxes
    ;; (so we cannot rename them either)
    ((r7c heap pair)
     pair? cons car cdr set-car! set-cdr! null?
     caar cadr cdar cddr)
    ((r7c heap eqv)
     eqv?)
    ((r7c heap core)
     eq?)
    ((r7c heap eof-object)
     eof-object? eof-object)
    ((r7c heap boolean)
     not boolean? 
     $boolean=?)
    ((r7c heap procedure)
     procedure?)
    ((r7c heap char)
     char? char->integer integer->char 
     $char=?)
    ((r7c heap string)
     string? string-length string-ref string-set!
     $make-string)
    ((r7c heap bytevector)
     bytevector? bytevector-length bytevector-u8-ref bytevector-u8-set!
     $make-bytevector)
    ((r7c heap symbol)
     symbol? symbol->string string->symbol
     $symbol=?)
    ((r7c heap vector)
     vector? vector-length vector-ref vector-set!
     $make-vector)
    ((r7c heap listloop)
     memv
     $fx-length $append)
    ((r7c heap fixnum)
     $fx= $fx<= $fx>= $fx< $fx>
     $fx+ $fx-)
    ((r7c heap undefined)
     $undefined)
    ((r7c core error)
     error)
    ((r7c core apply)
     apply)
    ((r7c core values)
     values call-with-values)
    ((r7c core exception)
     raise raise-continuable with-exception-handler)
    ((r7c core callcc)
     call-with-current-continuation call/cc)))
         
)

(library (yunivm util r7cmapping)
         (export
           r7cmapping/stdlib
           r7cmapping/coreproc)
         (import (yuni scheme))

(define r7cmapping/stdlib
  '(;; Standard libraries
    ((r7c-basic lib boolean)
     boolean=?)

    ((r7c-basic lib char)
     char=? char<? char>? char<=? char>=?)

    ((r7c-basic lib cxr)
     caaaar caaadr caaar caadar caaddr caadr 
     cadaar cadadr cadar caddar cadddr caddr
     cdaaar cdaadr cdaar cdadar cdaddr cdadr 
     cddaar cddadr cddar cdddar cddddr cdddr)

    ((r7c-basic lib lists)
     list?
     list append reverse
     memq
     assq assv
     make-list length list-tail list-ref list-set! list-copy)

    ((r7c-basic lib strings)
     string substring
     string-append string->list list->string
     string-copy string-copy! string-fill!
     make-string
     string=?
     string<? string>? string<=? string>=?)

    ((r7c-basic lib vectors)
     vector
     vector->list list->vector
     vector->string string->vector
     vector-copy vector-copy! vector-append vector-fill!
     make-vector)

    ((r7c-basic lib bytevectors)
     bytevector
     bytevector-copy bytevector-copy! bytevector-append
     utf8->string string->utf8
     make-bytevector)

    ((r7c-basic lib mapforeach)
     map string-map vector-map
     for-each string-for-each vector-for-each)

    ((r7c-equiv std equal)
     equal?)

    ((r7c-equiv std lists)
     member assoc)

    ((r7c-numeric std inexact)
     acos asin atan cos
     exp finite? infinite? log
     nan? sin sqrt tan)

    ((r7c-numeric std generic)
     = < > <= >=
     + * - / 
     number? complex? real? integer?
     exact? inexact? exact-integer?
     quotient remainder modulo
     floor ceiling truncate round
     expt
     inexact exact)

    ((r7c-numeric std misc)
     zero? positive? negative? odd? even?
     square max min abs
     gcd lcm)

    ((r7c-numeric std division)
     floor/ floor-quotient floor-remainder
     truncate/ truncate-quotient truncate-remainder)

    ((r7c-numeric std integer-sqrt)
     exact-integer-sqrt)

    ((r7c-numeric std reader)
     string->number)

    ((r7c-numeric std writer)
     number->string)

    ((r7c-exceptions std error)
     error-object? error-object-message error-object-irritants)

    ((r7c-io port core)
     port?
     input-port? output-port? textual-port? binary-port?)

    ((r7c-io port control)
     flush-output-port
     input-port-open? output-port-open?
     close-port close-input-port close-output-port)

    ((r7c-io port objects)
     read-char peek-char read-line
     read-string
     read-u8 peek-u8
     read-bytevector read-bytevector!
     newline
     write-char write-string write-u8 write-bytevector)

    ((r7c-io port files)
     open-input-file open-binary-input-file
     open-output-file open-binary-output-file)

    ((r7c-io port buffers)
     open-input-string open-output-string get-output-string
     open-input-bytevector open-output-bytevector get-output-bytevector)

    ((r7c-io port defaults)
     current-input-port current-output-port current-error-port
     with-input-from-file with-output-to-file)

    ((r7c-io port util)
     call-with-port call-with-input-file call-with-output-file)

    ((r7c-io port exceptions)
     read-error? file-error?)

    ((r7c-io reader datum)
     read)

    ((r7c-io writer datum)
     write write-shared write-simple
     display)

    ((r7c-io system files)
     file-exists? delete-file)

    ((r7c-os std process)
     exit
     command-line
     get-environment-variable)

    ((r7c-xxx std unimplemented)
     dynamic-wind make-parameter)))

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
     $fx+ $fx- $fx* $fx/
     $fx->fl
     $fx-expt
     ;; 2 values div+mod
     $fx-floor/
     $fx-truncate/)
    ((r7c heap flonum)
     $fl-nan? $fl-finite? $fl-infinite?
     $fl= $fl<= $fl>= $fl< $fl>
     $fl+ $fl- $fl* $fl/
     $fl->fx
     $fl-expt
     $fl-floor $fl-ceiling $fl-truncate $fl-round
     $fl-acos $fl-asin $fl-atan
     $fl-cos  $fl-sin  $fl-tan
     $fl-exp $fl-log $fl-sqrt
     ;; 2 values div+mod
     $fl-floor/
     $fl-truncate/)
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

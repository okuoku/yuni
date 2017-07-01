(library (yunivmrt scheme-proc)
         (export

* + - / < <= = > >= abs append apply assoc assq assv 
binary-port? boolean=? boolean? bytevector bytevector-append bytevector-copy
bytevector-copy! bytevector-length bytevector-u8-ref bytevector-u8-set!
bytevector? caar cadr call-with-current-continuation call-with-port
call-with-values call/cc car cdar cddr cdr ceiling char->integer

char<=?  char<? char=? char>=? char>? char? close-input-port
close-output-port close-port complex?

cons current-error-port
current-input-port current-output-port

dynamic-wind eof-object eof-object? eq?
equal? eqv?  error error-object-irritants error-object-message error-object?
even? exact exact-integer-sqrt exact-integer? exact? expt 

file-error?
floor floor-quotient floor-remainder floor/ flush-output-port for-each gcd
get-output-bytevector get-output-string

inexact inexact? input-port-open? input-port? integer->char integer? lcm
length
list list->string list->vector list-copy list-ref list-set! list-tail list?
make-bytevector make-list make-parameter make-string make-vector map max member
memq memv min modulo negative? newline not null? number->string number?

odd? open-input-bytevector open-input-string open-output-bytevector
open-output-string output-port-open? output-port? pair?
peek-char peek-u8 port? positive?  procedure? quotient raise
raise-continuable 

read-bytevector read-bytevector!
read-char read-error?  read-line read-string read-u8 real? remainder reverse
round set-car! set-cdr! square string string->list string->number
string->symbol string->utf8 string->vector string-append string-copy
string-copy! string-fill! string-for-each string-length string-map string-ref
string-set! string<=? string<? string=? string>=? string>? string? substring
symbol->string symbol=? symbol? textual-port? truncate
truncate-quotient truncate-remainder truncate/ 

utf8->string values vector vector->list vector->string
vector-append vector-copy vector-copy! vector-fill! vector-for-each
vector-length vector-map vector-ref vector-set!  vector? 
with-exception-handler write-bytevector write-char write-string write-u8 zero?

caaaar caaadr caaar caadar caaddr caadr cadaar cadadr cadar caddar cadddr caddr
cdaaar cdaadr cdaar cdadar cdaddr cdadr cddaar cddadr cddar cdddar cddddr cdddr

call-with-input-file call-with-output-file delete-file file-exists?
open-binary-input-file open-binary-output-file open-input-file
open-output-file with-input-from-file with-output-to-file

acos asin atan cos exp finite? log nan? sin sqrt tan

command-line exit get-environment-variable

read
display write write-simple
%%yunifake-dummy-syntax-procs
           )
         (import 
           ;; FIXME: Integrate these into r7c-* libraries
           (r7c heap boolean)
           (r7c heap procedure)
           (r7c heap pair)
           (r7c heap list)
           (r7c heap eqv)
           (r7c heap core)
           (r7c heap char)
           (r7c heap string)
           (r7c heap bytevector)
           (r7c heap symbol)
           (r7c heap eof-object)
           (r7c heap vector)
           (r7c heap undefined)
           (r7c heap listloop)
           (r7c heap fixnum)
           (r7c core values)
           (r7c core apply)
           (r7c core error)
           (r7c core callcc)
           (r7c core exception)

           ;; Standard libraries
           (r7c-basic lib boolean)
           (r7c-basic lib char)
           (r7c-basic lib cxr)
           (r7c-basic lib lists)
           (r7c-basic lib strings)
           (r7c-basic lib vectors)
           (r7c-basic lib bytevectors)
           (r7c-basic lib mapforeach)
           (r7c-equiv std equal)
           (r7c-equiv std lists)
           (r7c-numeric std inexact)
           (r7c-numeric std generic)
           (r7c-numeric std misc)
           (r7c-numeric std division)
           (r7c-numeric std integer-sqrt)
           (r7c-numeric std reader)
           (r7c-numeric std writer)
           (r7c-exceptions std error)
           (r7c-io port core)
           (r7c-io port control)
           (r7c-io port objects)
           (r7c-io port files)
           (r7c-io port buffers)
           (r7c-io port defaults)
           (r7c-io port util)
           (r7c-io port exceptions)
           (r7c-io reader datum)
           (r7c-io writer datum)
           (r7c-io system files)
           (r7c-os std process)
           (r7c-xxx std unimplemented))
         
;; Mark explicitly as a macro-library
(define-syntax-names/yunifake %%yunifake-dummy-syntax-procs)         
         )

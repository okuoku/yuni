("4.1.2. Literal expressions"
 quote)
("4.1.4. Procedures"
 lambda)
("4.1.5. Conditionals"
 if)
("4.1.6. Assignments" set!)
("4.1.7. Inclusion" include include-ci)
("4.2.1. Conditionals" 
 cond else => case
 and or when unless
 cond-expand)
("4.2.2. Binding constructs"
 let let* letrec letrec*
 let-values let*-values)
("4.2.3. Sequencing" begin)

("4.2.4. Iteration" do) ;; let

("4.2.5. Delayed evaluation"
 delay delay-force force
 promise? make-promise)

("4.2.6. Dynamic bindings" make-parameter parameterize)

("4.2.7. Exception handling" guard)

("4.2.8. Quasiquotation" quasiquote unquote unquote-splicing)

("4.2.9. Case-lambda" case-lambda)

("4.3.1. Binding constructs for syntactic keywords"
 let-syntax letrec-syntax)
("4.3.2. Pattern language" syntax-rules _ ...)

("4.3.3. Signaling errors in macro transformers"
 syntax-error)

("5.3. Variable definitions" define)

("5.3.3. Multiple-value definitions" define-values)

("5.4. Syntax definitions" define-syntax)

("5.5. Record-type definitions" define-record-type)

("6.1. Equivalence predicates" eqv? eq? equal?)

("6.2.6. Numerical operations"
 number?
 complex?
 real?
 rational?
 integer?

 exact?
 inexact?
 exact-integer?
 finite?
 infinite?
 nan?
 = < > <= >=
 zero? positive? negative? odd? even?
 max min
 + *
 - /
 abs
 floor/
 floor-quotient
 floor-remainder
 truncate/
 truncate-quotient
 truncate-remainder
 quotient
 remainder
 modulo
 gcd
 lcm
 numerator
 denominator
 floor
 ceiling
 truncate
 round
 rationalize
 exp
 log
 sin
 cos
 tan
 asin
 acos
 atan
 square
 sqrt
 exact-integer-sqrt
 expt
 make-rectangular
 make-polar
 real-part
 imag-part
 magnitude
 angle
 inexact
 exact)

("6.2.7. Numerical input and output"
 number->string string->number)

("6.3. Booleans" not boolean? boolean=?)

("6.4. Pairs and lists"
 pair? cons car cdr set-car! set-cdr! caar cadr cdar cddr
caaaar caaadr
caaar caadar
caaddr caadr
cadaar cadadr
cadar caddar
cadddr caddr
cdaaar cdaadr
cdaar cdadar
cdaddr cdadr
cddaar cddadr
cddar cdddar
cddddr cdddr
 null? list? make-list list length append reverse list-tail list-ref
 list-set! memq memv member assq assv assoc 
 list-copy)

("6.5. Symbols"
 symbol? symbol=? symbol->string string->symbol)

("6.6. Characters"
 char? char=? char<? char>? char<=? char>=?
 char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
 char-alphabetic? char-numeric? char-whitespace? char-upper-case?
 char-lower-case?
 digit-value
 char->integer
 integer->char
 char-upcase char-downcase char-foldcase)

("6.7. Strings"
 string? make-string string string-length string-ref string-set!
 string=? string-ci=?
 string<? string-ci<?
 string>? string-ci>?
 string<=?
 string>=?
 string-ci<=?
 string-ci>=?
 string-upcase
 string-downcase
 string-foldcase
 substring
 string-append
 string->list
 list->string
 string-copy
 string-copy!
 string-fill!)

("6.8. Vectors"
 vector? make-vector
 vector vector-length vector-ref vector-set! 
 vector->list list->vector
 vector->string string->vector
 vector-copy vector-copy!
 vector-append
 vector-fill!)

("6.9. Bytevectors"
 bytevector?
 make-bytevector
 bytevector
 bytevector-length
 bytevector-u8-ref
 bytevector-u8-set!
 bytevector-copy
 bytevector-copy!
 bytevector-append
 utf8->string
 string->utf8)

("6.10. Control features"
 procedure?
 apply
 map
 string-map
 vector-map
 for-each
 string-for-each
 vector-for-each
 call-with-current-continuation
 call/cc
 values
 call-with-values
 dynamic-wind)

("6.11. Exceptions"
 with-exception-handler
 raise
 raise-continuable
 error
 error-object?
 error-object-message
 error-object-irritants
 read-error?
 file-error?)

("6.12. Environments and evaluation"
 environment scheme-report-environment
 null-environment interaction-environment
 eval)

("6.13.1. Ports"
 call-with-port
 call-with-input-file
 call-with-output-file
 input-port?
 output-port?
 textual-port?
 binary-port?
 port?
 input-port-open?
 output-port-open?
 current-input-port
 current-output-port
 current-error-port
 with-input-from-file
 with-output-to-file
 open-input-file
 open-binary-input-file
 open-output-file
 open-binary-output-file
 close-port
 close-input-port
 close-output-port
 open-input-string
 open-output-string
 get-output-string
 open-input-bytevector
 open-output-bytevector
 get-output-bytevector)

("6.13.2. Input"
 read
 read-char
 peek-char
 read-line
 eof-object?
 eof-object
 char-ready?
 read-string
 read-u8
 peek-u8
 u8-ready?
 read-bytevector
 read-bytevector!)

("6.13.3. Output"
 write
 write-shared
 write-simple
 display
 newline
 write-char
 write-string
 write-u8
 write-bytevector
 flush-output-port)

("6.14. System interface"
 load
 file-exists?
 delete-file
 command-line
 exit
 emergency-exit
 get-environment-variable
 get-environment-variables
 current-second
 current-jiffy
 jiffies-per-second
 features)

((scheme base)
 * +
- ...
/ <
<= =
=> >
>=
_
abs and
append apply
assoc assq
assv begin
binary-port? boolean=?
boolean? bytevector
bytevector-append bytevector-copy
bytevector-copy! bytevector-length
bytevector-u8-ref bytevector-u8-set!
bytevector? caar
cadr
call-with-current-continuation
call-with-port call-with-values
call/cc car
case cdar
cddr cdr
ceiling char->integer
char-ready? char<=?
char<? char=?
char>=? char>?
char? close-input-port
close-output-port close-port
complex? cond
cond-expand cons
current-error-port current-input-port
current-output-port define
define-record-type define-syntax
define-values denominator
do dynamic-wind
else eof-object
eof-object? eq?
equal? eqv?
error error-object-irritants
error-object-message error-object?
even? exact
exact-integer-sqrt exact-integer?
exact? expt
features file-error?
floor floor-quotient
floor-remainder floor/
flush-output-port for-each
gcd get-output-bytevector
get-output-string guard
if include
include-ci inexact
inexact? input-port-open?
input-port? integer->char
integer? lambda
lcm length
let let*
let*-values let-syntax
let-values letrec
letrec* letrec-syntax
list list->string
list->vector list-copy
list-ref list-set!
list-tail list?
make-bytevector make-list
make-parameter make-string
make-vector map
max member
memq memv
min modulo
negative? newline
not null?
number->string number?
numerator odd?
open-input-bytevector open-input-string
open-output-bytevector open-output-string
or output-port-open?
output-port? pair?
parameterize peek-char
peek-u8 port?
positive? procedure?
quasiquote quote
quotient raise
raise-continuable rational?
rationalize read-bytevector
read-bytevector! read-char
read-error? read-line
read-string read-u8
real? remainder
reverse round
set! set-car!
set-cdr! square
string string->list
string->number string->symbol
string->utf8 string->vector
string-append string-copy
string-copy! string-fill!
string-for-each string-length
string-map string-ref
string-set! string<=?
string<? string=?
string>=? string>?
string? substring
symbol->string symbol=?
symbol? syntax-error
syntax-rules textual-port?
truncate truncate-quotient
truncate-remainder truncate/
u8-ready? unless
unquote unquote-splicing
utf8->string values
vector vector->list
vector->string vector-append
vector-copy vector-copy!
vector-fill! vector-for-each
vector-length vector-map
vector-ref vector-set!
vector? when
with-exception-handler write-bytevector
write-char write-string
write-u8 
zero?)

((scheme case-lambda) case-lambda)

((scheme char)
char-alphabetic? char-ci<=?
char-ci<? char-ci=?
char-ci>=? char-ci>?
char-downcase char-foldcase
char-lower-case? char-numeric?
char-upcase char-upper-case?
char-whitespace? digit-value
string-ci<=? string-ci<?
string-ci=? string-ci>=?
string-ci>? string-downcase
string-foldcase string-upcase
 )

((scheme complex)
 angle magnitude make-rectangular imag-part make-polar real-part)

((scheme cxr)
caaaar caaadr
caaar caadar
caaddr caadr
cadaar cadadr
cadar caddar
cadddr caddr
cdaaar cdaadr
cdaar cdadar
cdaddr cdadr
cddaar cddadr
cddar cdddar
cddddr cdddr
 )

((scheme eval)
 environment eval )

((scheme file)
call-with-input-file call-with-output-file
delete-file file-exists?
open-binary-input-file open-binary-output-file
open-input-file open-output-file
with-input-from-file with-output-to-file
 )

((scheme inexact)
cos asin
atan acos
exp finite?
infinite? log
nan? sin
sqrt tan)

((scheme lazy)
delay delay-force
force make-promise
promise?
 )
((scheme load) load)

((scheme process-context)
command-line emergency-exit
exit
get-environment-variable
get-environment-variables
 )

((scheme read) read)

((scheme time)
 current-jiffy current-second jiffies-per-second)

((scheme write) display write write-shared write-simple)

#|
((scheme r5rs)
* +
- /
< <=
= >
>= abs
acos and
angle append
apply asin
assoc assq
assv atan
begin boolean?
caaaar caaadr
caaar caadar
caaddr caadr
caar cadaar
cadadr cadar
caddar cadddr
caddr cadr
call-with-current-continuation
call-with-input-file call-with-output-file
call-with-values car
case cdaaar
cdaadr cdaar
cdadar cdaddr
cdadr cdar
cddaar cddadr
cddar cdddar
cddddr cdddr
cddr cdr
ceiling char->integer
char-alphabetic? char-ci<=?
char-ci<? char-ci=?
char-ci>=? char-ci>?
char-downcase char-lower-case?
char-numeric? char-ready?
char-upcase char-upper-case?
char-whitespace? char<=?
char<? char=?
char>=? char>?
char? close-input-port
close-output-port complex?
cond cons
cos current-input-port
current-output-port define
define-syntax delay
denominator display
do dynamic-wind
eof-object? eq?
equal? eqv?
eval even?
exact->inexact exact?
exp expt
floor for-each
force gcd
if imag-part
inexact->exact inexact?
input-port? integer->char
integer? interaction-environment
lambda lcm
length let
let* let-syntax
letrec letrec-syntax
list list->string
list->vector list-ref
list-tail list?
load log
magnitude make-polar
make-rectangular make-string
make-vector map
max member
memq memv
min modulo
negative? newline
not null-environment
null? number->string
number? numerator
odd? open-input-file
open-output-file or
output-port? pair?
peek-char positive?
procedure? quasiquote
quote quotient
rational? rationalize
read read-char
real-part real?
remainder reverse
round
scheme-report-environment
set! set-car!
set-cdr! sin
sqrt string
string->list string->number
string->symbol string-append
string-ci<=? string-ci<?
string-ci=? string-ci>=?
string-ci>? string-copy
string-fill! string-length
string-ref string-set!
string<=? string<?
string=? string>=?
string>? string?
substring symbol->string
symbol? tan
truncate values
vector vector->list
vector-fill! vector-length
vector-ref vector-set!
vector? with-input-from-file
with-output-to-file write
write-char zero?
 )
|#

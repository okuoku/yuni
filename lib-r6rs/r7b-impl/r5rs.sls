#!r6rs
(library (r7b-impl r5rs)
        (export 
;; From r7rs 9th draft
* + - /
< <= =>
> = >= abs
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
caddr cadr call-with-current-continuation call-with-input-file call-with-output-file call-with-values car
case                     cdaaar
cdaadr                   cdaar
cdadar                   cdaddr
cdadr                    cdar
cddaar                   cddadr
cddar                    cdddar
cddddr                   cdddr
cddr                     cdr
ceiling                  char->integer
char-alphabetic?         char-ci<=?
char-ci<?                char-ci=?
char-ci>=?               char-ci>?
char-downcase            char-lower-case?
char-numeric?            char-ready?
char-upcase              char-upper-case?
char-whitespace?         char<=?
char<?                   char=?
char>=?                  char>?
char?                    close-input-port
close-output-port        complex?
cond                     cons
cos                      current-input-port
current-output-port      define
define-syntax            delay
denominator              display
do                       dynamic-wind
eof-object?              eq?
equal?                   eqv?
eval                     even?
exact->inexact           exact?
exp                      expt
floor                    for-each
force                    gcd
if                       imag-part
inexact->exact           inexact?
input-port?              integer->char
integer?                 interaction-environment
lambda                   lcm
length                   let

let*
     letrec
     list
     list->vector
     list-tail
     load
     magnitude
     make-rectangular
     make-vector
     max
     memq
     min
     negative?
     not
     null?
     number?
     odd?
     open-output-file
     output-port?
     peek-char
     procedure?
     quote
     rational?
     read
     real-part
     remainder
     round
     scheme-report-environment
     set!
     set-cdr!
     sqrt
     string->list
     string->symbol
     string-ci<=?
     string-ci=?
     string-ci>?
     string-fill!
     string-ref
     string<=?
     string=?
     string>?
     substring
     symbol?
     truncate
     vector
     vector-fill!
     vector-ref
     vector?
     with-output-to-file
     write-char
let-syntax
letrec-syntax
list->string
list-ref
list?
log
make-polar
make-string
map
member
memv
modulo
newline
null-environment
number->string
numerator
open-input-file
or
pair?
positive?
quasiquote
quotient
rationalize
read-char
real?
reverse
set-car!
sin
string
string->number
string-append
string-ci<?
string-ci>=?
string-copy
string-length
string-set!
string<?
string>=?
string?
symbol->string
tan
values
vector->list
vector-length
vector-set!
with-input-from-file
write
zero?
          )
        (import (except (rnrs r5rs) delay force) 
                (r7b-impl base) 
                (r7b-impl inexact)
                (r7b-impl complex)
                (r7b-impl cxr)
                (r7b-impl file)
                (r7b-impl char)
                (r7b-impl read)
                (r7b-impl write)
                (r7b-impl eval)
                (r7b-impl repl)
                (r7b-impl load)
                (r7b-impl lazy)))

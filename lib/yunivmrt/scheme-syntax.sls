(library (yunivmrt scheme-syntax)
         (export
_ ... => and begin case
cond 

define define-record-type define-syntax define-values 

do else guard if 

lambda let let* let*-values let-syntax let-values letrec letrec* letrec-syntax

or parameterize quasiquote quote

set! syntax-error syntax-rules

unless unquote unquote-splicing

when case-lambda)
         (import (yunifake standard-procs)))

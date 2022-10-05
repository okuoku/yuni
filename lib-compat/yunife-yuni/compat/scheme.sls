(library (yunife-yuni compat scheme)
         (export
           ;; Expander syntax
           define-syntax 
           syntax-rules ... => _ else
           syntax-error 
           
           ;; yunivm core syntax
           if when lambda begin 
           set! quote letrec*

           ;; Binding (yunife-yunivm runtime define)
           define 

           ;; Binding (yunife-yunivm runtime let)
           let let* letrec 

           ;; Binding (yunife-yunivm runtime let-values)
           let*-values let-values 

           ;; runtime syntax (yunife-yunivm runtime syntax)
           and or unless 
           case cond do 

           ;; runtime syntax (yunife-yunivm runtime qq)
           quasiquote unquote unquote-splicing 

           ;; Case-lambda (yunife-yunivm runtime case-lambda)
           case-lambda 

           ;; Unsupported (yunife-yunivm runtime unsupported)
           let-syntax letrec-syntax
           define-record-type define-values
           guard 
           parameterize

           ;; from R7RS draft 7
           * + - / < <= = > >= abs 
           append apply assoc assq assv 
           binary-port? boolean=? boolean? bytevector 
           bytevector-append bytevector-copy bytevector-copy! 
           bytevector-length bytevector-u8-ref bytevector-u8-set!
           bytevector? 
           caar cadr 
           call-with-current-continuation call/cc 
           call-with-port
           call-with-values 
           car cdar cddr cdr ceiling char->integer
           char<=?  char<? char=? char>=? char>? char? close-input-port
           close-output-port close-port complex? 
           cons current-error-port
           current-input-port current-output-port 
           dynamic-wind eof-object eof-object? eq?
           equal? eqv? 
           error error-object-irritants error-object-message error-object?
           even? exact exact-integer-sqrt exact-integer? exact? expt
           file-error?
           floor floor-quotient floor-remainder floor/ flush-output-port 
           for-each gcd
           get-output-bytevector get-output-string 
           inexact inexact? input-port-open? input-port? 
           integer->char integer? lcm
           length list list->string list->vector list-copy 
           list-ref list-set! list-tail list?
           make-bytevector make-list make-parameter 
           make-string make-vector map max member
           memq memv min modulo negative? newline not null? 
           number->string number? odd? open-input-bytevector 
           open-input-string open-output-bytevector
           open-output-string 
           output-port-open? output-port? pair?
           peek-char peek-u8 port? positive?  procedure? 
           quotient raise raise-continuable
           read-bytevector read-bytevector!
           read-char read-error? read-line read-string read-u8 
           real? remainder reverse
           round set-car! set-cdr! square string string->list string->number
           string->symbol string->utf8 string->vector string-append string-copy
           string-copy! string-fill! string-for-each 
           string-length string-map string-ref
           string-set! string<=? string<? 
           string=? string>=? string>? string? substring
           symbol->string symbol=? symbol? 
           textual-port? truncate
           truncate-quotient truncate-remainder truncate/
           utf8->string values vector vector->list vector->string
           vector-append vector-copy vector-copy! vector-fill! vector-for-each
           vector-length vector-map vector-ref vector-set!  vector? 
           with-exception-handler write-bytevector 
           write-char write-string write-u8 zero?


           ;; from R7RS draft 7
           caaaar caaadr caaar caadar caaddr caadr 
           cadaar cadadr cadar caddar cadddr caddr
           cdaaar cdaadr cdaar cdadar cdaddr cdadr 
           cddaar cddadr cddar cdddar cddddr cdddr

           ;; from R7RS draft 4
           call-with-input-file call-with-output-file delete-file file-exists?
           open-binary-input-file open-binary-output-file open-input-file
           open-output-file with-input-from-file with-output-to-file
 
           acos asin atan cos exp finite? log nan? sin sqrt tan
 
           ;; from R7RS draft 4
           command-line exit get-environment-variable
           read
           display write write-simple
           )
         (import 
           (yunivm-core-syntax)
           (yunife-yunivm runtime case-lambda)
           (yunife-yunivm runtime define)
           (yunife-yunivm runtime let-values)
           (yunife-yunivm runtime let)
           (yunife-yunivm runtime qq)
           (yunife-yunivm runtime syntax)
           (yunife-yunivm runtime unsupported)))

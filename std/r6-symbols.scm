((rnrs base)
 ;; 11.2.1 Variable definitions
 define
 define-syntax

 ;; 11.4.1 Quotation
 quote

 ;; 11.4.2 Procedures
 lambda

 ;; 11.4.3 Conditionals
 if

 ;; 11.4.4 Assignments
 set!

 ;; 11.4.5 Derived conditionals
 cond
 =>
 else
 case
 and
 or

 ;; 11.4.6 Binding constructs
 let
 let*
 letrec
 letrec*
 let-values
 let*-values

 ;; 11.4.7 Sequencing
 begin

 ;; 11.5 Equivalence predicates
 eqv?
 eq?
 equal?

 ;; 11.6 Procedure predicate
 procedure?

 ;; 11.7.4 Numerical operations
 number?
 complex?
 real?
 rational?
 integer?
 real-valued?
 rational-valued?
 integer-valued?
 exact?
 inexact?
 inexact
 exact
 =
 <
 >
 <=
 >=
 zero?
 positive?
 negative?
 odd?
 even?
 finite?
 infinite?
 nan?
 max
 min
 +
 *
 -
 /
 abs
 div-and-mod
 div
 mod
 div0-and-mod0
 div0
 mod0
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
 sqrt
 exact-integer-sqrt
 expt
 make-rectangular
 make-polar
 real-part
 imag-part
 magnitude
 angle
 number->string
 string->number
 ;; 11.8 Booleans
 not
 boolean?
 boolean=?

 ;; 11.9 Pairs and lists
 pair?
 cons
 car
 cdr
 caar cadr cdar cddr
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
 null?
 list?
 list
 length
 append
 reverse
 list-tail
 list-ref
 map
 for-each

 ;; 11.10 Symbols
 symbol?
 symbol->string
 symbol=?
 string->symbol

 ;; 11.11 Characters
 char?
 char->integer
 integer->char

 char=?
 char<?
 char>?
 char<=?
 char>=?

 ;; 11.12 Strings
 string?
 make-string
 string
 string-length
 string-ref
 string=?
 string<?
 string>?
 string<=?
 string>=?

 substring
 string-append
 string->list
 list->string
 string-for-each
 string-copy

 ;; 11.13 Vectors
 vector?
 make-vector
 vector
 vector-length
 vector-ref
 vector-set!
 vector->list
 list->vector
 vector-fill!
 vector-map
 vector-for-each

 ;; 11.14 Errors and violations
 error
 assertion-violation
 assert

 ;; 11.15 Control features
 apply
 call-with-current-continuation
 call/cc
 values
 call-with-values
 dynamic-wind

 ;; 11.16 Iteration
 let

 ;; 11.17 Quasiquotation
 quasiquote
 unquote
 unquote-splicing

 ;; 11.18 Binding constructs for syntactic keywords
 let-syntax
 letrec-syntax

 ;; 11.19 Macro transformers
 syntax-rules
 _
 ...
 identifier-syntax
 set!)

;;;; Standard libraris

((rnrs unicode)
 ;; 1.1 Characters
 char-upcase
 char-downcase
 char-titlecase
 char-foldcase
 char-ci=?
 char-ci<?
 char-ci>?
 char-ci<=?
 char-ci>=?
 char-alphabetic?
 char-numeric?
 char-whitespace?
 char-upper-case?
 char-lower-case?
 char-title-case?
 char-general-category

 ;; 1.2 Strings
 string-upcase
 string-downcase
 string-titlecase
 string-foldcase
 string-ci=?
 string-ci<?
 string-ci>?
 string-ci<=?
 string-ci>=?

 string-normalize-nfd
 string-normalize-nfkd
 string-normalize-nfc
 string-normalize-nfkc)

((rnrs bytevectors)
 ;; 2.2 General operations
 endianness
 native-endianness
 bytevector?
 make-bytevector
 bytevector-length
 bytevector=?
 bytevector-fill!
 bytevector-copy!
 bytevector-copy

 ;; 2.3 Operations on bytes and octets
 bytevector-u8-ref
 bytevector-s8-ref
 bytevector-u8-set!
 bytevector-s8-set!
 bytevector->u8-list
 u8-list->bytevector

 ;; 2.4 Operations on integers of arbitrary size
 bytevector-uint-ref
 bytevector-sint-ref
 bytevector-uint-set!
 bytevector-sint-set!
 bytevector->uint-list
 bytevector->sint-list
 uint-list->bytevector
 sint-list->bytevector

 ;; 2.5 Operations on 16-bit integers
 bytevector-u16-ref
 bytevector-s16-ref
 bytevector-u16-native-ref
 bytevector-s16-native-ref
 bytevector-u16-set!
 bytevector-s16-set!
 bytevector-u16-native-set!
 bytevector-s16-native-set!

 ;; 2.6 Operations on 32-bit integers
 bytevector-u32-ref
 bytevector-s32-ref
 bytevector-u32-native-ref
 bytevector-s32-native-ref
 bytevector-u32-set!
 bytevector-s32-set!
 bytevector-u32-native-set!
 bytevector-s32-native-set!

 ;; 2.7 Operations on 64-bit integers
 bytevector-u64-ref
 bytevector-s64-ref
 bytevector-u64-native-ref
 bytevector-s64-native-ref
 bytevector-u64-set!
 bytevector-s64-set!
 bytevector-u64-native-set!
 bytevector-s64-native-set!

 ;; 2.8 Operations on IEEE-754 representations
 bytevector-ieee-single-native-ref
 bytevector-ieee-single-ref
 bytevector-ieee-double-native-ref
 bytevector-ieee-double-ref
 bytevector-ieee-single-native-set!
 bytevector-ieee-single-set!
 bytevector-ieee-double-native-set!
 bytevector-ieee-double-set!

 ;; 2.9 Operations on strings
 string->utf8
 string->utf16
 string->utf32
 utf8->string
 utf16->string
 utf32->string)

((rnrs lists)
 ;; 3 List utilities
 find
 for-all
 exists
 filter
 partition
 fold-left
 fold-right
 remp
 remove
 remv
 remq
 memp
 member
 memv
 memq
 assp
 assoc
 assv
 assq
 cons*)

((rnrs sorting)
 ;; 4 Sorting
 list-sort
 vector-sort
 vector-sort!)

((rnrs control)
 ;; 5 Control structures
 when
 unless
 do
 case-lambda)

((rnrs records syntactic)
 ;; 6.2 Syntactic layer
 define-record-type
 fields
 mutable
 immutable
 parent
 protocol
 sealed
 opaque
 nongenerative
 parent-rtd
 record-type-descripter
 record-constructor-descriptor)

((rnrs records procedural)
 ;; 6.3 Procedural layer
 make-record-type-descriptor
 record-type-descriptor?
 make-record-constructor-descriptor
 record-constructor
 record-predicate
 record-accessor
 record-mutator)

((rnrs records inspection)
 ;; 6.4 Inspection
 record?
 record-rtd
 record-type-name
 record-type-parent
 record-type-uid
 record-type-generative?
 record-type-sealed?
 record-type-opaque?
 record-type-field-names
 record-field-mutable?)

((rnrs exceptions)
 ;; 7.1 Exceptions
 with-exception-handler
 guard
 =>
 else
 raise
 raise-continuable)

((rnrs conditions)
 ;; 7.2 Conditions
 &condition
 condition
 simple-conditions
 condition?
 condition-predicate
 condition-accessor
 define-condition-type

 ;; 7.3 Standard condition types
 &message
 make-message-condition
 message-condition?
 condition-message

 &warning
 make-warning
 warning?

 &serious
 make-serious-condition
 serious-condition?

 &error
 make-error
 error?

 &violation
 make-violation
 violation?

 &assertion
 make-assertion-violation
 assertion-violation?

 &irritants
 make-irritants-condition
 irritants-condition?
 condition-irritants

 &who
 make-who-condition
 who-condition?
 condition-who

 &non-continuable
 make-non-continuable-violation
 non-continuable-violation?

 &implementation-restriction
 make-implementation-restriction-violation
 implementation-restriction-violation?

 &lexical
 make-lexical-violation
 lexical-violation?

 &syntax
 make-syntax-violation
 syntax-violation?
 syntax-violation-form
 syntax-violation-subform

 &undefined
 make-undefined-violation
 undefined-violation?)

((rnrs io ports) ;; and simple and files ...
 ;; 8.1 Condition types
 &i/o
 make-i/o-error
 i/o-error?

 &i/o-read
 make-i/o-read-error
 i/o-read-error?

 &i/o-write
 make-i/o-write-error
 i/o-write-error?

 &i/o-invalid-position
 make-i/o-invalid-position-error
 i/o-invalid-position-error?
 i/o-error-position

 &i/o-filename
 make-i/o-filename-error
 i/o-filename-error?
 i/o-error-filename

 &i/o-file-protection
 make-i/o-file-protection-error
 i/o-file-protection-error?

 &i/o-file-is-read-only
 make-i/o-file-is-read-only-error
 i/o-file-is-read-only-error?

 &i/o-file-already-exists
 make-i/o-file-already-exists-error
 i/o-file-already-exists-error?

 &i/o-file-does-not-exist
 make-i/o-file-does-not-exist-error
 i/o-file-does-not-exist-error?

 &i/o-port
 make-i/o-port-error
 i/o-port-error?
 i/o-error-port

 ;(rnrs io ports) ;; continue
 ;; 8.2.2 File options
 file-options

 ;; 8.2.3 Buffer modes
 buffer-mode
 buffer-mode?

 ;; 8.2.4 Transcoders
 latin-1-codec
 utf-8-codec
 utf-16-codec
 eol-style
 native-eol-style

 &i/o-decoding
 make-i/o-decoding-error
 i/o-decoding-error?

 &i/o-encoding
 make-i/o-encoding-error
 i/o-encoding-error?
 i/o-encoding-error-char

 error-handling-mode

 make-transcoder
 native-transcoder
 transcoder-codec
 transcoder-eol-style
 transcoder-error-handling-mode
 bytevector->string
 string->bytevector

 ;; 8.2.5 End-of-file object
 eof-object
 eof-object?

 ;; 8.2.6 Input and output ports
 port?
 port-transcoder
 textual-port?
 binary-port?
 transcoded-port
 port-has-position?
 port-position
 port-has-set-port-position!?
 set-port-position!
 close-port
 call-with-port

 ;; 8.2.7 Input ports
 input-port?
 port-eof?
 open-file-input-port
 open-bytevector-input-port
 open-string-input-port
 standard-input-port
 current-input-port
 make-custom-binary-input-port
 make-custon-textual-input-port

 ;; 8.2.8 Binary input
 get-u8
 lookahead-u8
 get-bytevector-n
 get-bytevector-n!
 get-bytevector-some
 get-bytevector-all

 ;; 8.2.9 Textual input
 get-char
 lookahead-char
 get-string-n
 get-string-n!
 get-string-all
 get-line
 get-datum

 ;; 8.2.10 Output ports
 output-port?
 flush-output-port
 output-port-buffer-mode
 open-file-output-port
 open-bytevector-output-port
 call-with-bytevector-output-port
 open-string-output-port
 call-with-string-output-port
 standard-output-port
 standard-error-port
 current-output-port
 current-error-port
 make-custom-binary-output-port
 make-custom-textual-output-port

 ;; 8.2.11 Binary output
 put-u8
 put-bytevector

 ;; 8.2.12 Textual output
 put-char
 put-string
 put-datum

 ;; 8.2.13 Input/output ports
 open-file-input/output-port
 make-custom-binary-input/output-port
 make-custom-textual-input/output-port)

((rnrs io simple)
 ;; 8.3 Simple I/O
 eof-object
 eof-object?

 call-with-input-file
 call-with-output-file
 input-port?
 output-port?
 current-input-port
 current-output-port
 current-error-port
 with-input-from-file
 with-output-to-file
 open-input-file
 open-output-file
 close-input-port
 close-output-port
 read-char
 peek-char
 read
 write-char
 newline
 display
 write)

((rnrs files)
 ;; 9 File system
 file-exists?
 delete-file)

((rnrs programs)
 ;; 10 Command-line access and exit values
 command-line
 exit)

((rnrs arithmetic fixnums)
 ;; 11.2 Fixnums
 fixnum?
 fixnum-width
 least-fixnum
 greatest-fixnum
 fx=?
 fx>?
 fx<?
 fx>=?
 fx<=?

 fxzero?
 fxpositive?
 fxnegative?
 fxodd?
 fxeven?

 fxmax
 fxmin
 fx+
 fx*
 fx-
 fxdiv-and-mod
 fxdiv
 fxmod
 fxdiv0-and-mod0
 fxdiv0
 fxmod0
 fx+/carry
 fx-/carry
 fx*/carry
 fxnot
 fxand
 fxior
 fxxor
 fxif
 fxbit-count
 fxlength
 fxfirst-bit-set
 fxbit-set?
 fxcopy-bit
 fxbit-field
 fxcopy-bit-field
 fxarithmetic-shift
 fxarithmetic-shift-left
 fxarithmetic-shift-right
 fxrotate-bit-field
 fxreverse-bit-field)

((rnrs arithmetic flonums)
 ;; 11.3 Flonums
 flonum?
 real->flonum
 fl=?
 fl<?
 fl<=?
 fl>?
 fl>=?
 flinteger?
 flzero?
 flpositive?
 flnegative?
 flodd?
 fleven?
 flfinite?
 flinfinite?
 flnan?

 flmax
 flmin
 fl+
 fl*
 fl-
 fl/
 flabs
 fldiv-and-mod
 fldiv
 flmod
 fldiv0-and-mod0
 fldiv0
 flmod0

 flnumerator
 fldenominator

 flfloor
 flceiling
 fltruncate
 flround

 flexp
 fllog
 flsin
 flcos
 fltan
 flasin
 flacos
 flatan
 flsqrt

 &no-infinites
 make-no-infinites-violation
 no-infinites-violation?
 &no-nans
 make-no-nans-violation
 no-nans-violation?

 fixnum->flonum)

((rnrs arithmetic bitwise)
 ;; 11.4 Exact bitwise arithmetic
 bitwise-not
 bitwise-and
 bitwise-ior
 bitwise-xor
 bitwise-if
 bitwise-bit-count
 bitwise-length
 bitwise-first-bit-set
 bitwise-bit-set?
 bitwise-copy-bit
 bitwise-bit-field
 bitwise-copy-bit-field
 bitwise-arithmetic-shift
 bitwise-arithmetic-shift-left
 bitwise-arithmetic-shift-right
 bitwise-rotate-bit-field
 bitwise-reverse-bit-field)


((rnrs syntax-case)

 ;; 12.3 Transformers
 make-variable-transformer

 ;; 12.4 Parsing input and producing output
 syntax-case
 _
 ...
 syntax

 ;; 12.5 Identifier predicates
 identifier?
 bound-identifier=?
 free-identifier=?

 ;; 12.6 Syntax-object and datum conversions
 syntax->datum
 datum->syntax

 ;; 12.7 Generating lists of temporaries
 generate-temporaries

 ;; 12.8 Derived forms and procedures
 with-syntax
 quasisyntax
 unsyntax
 unsyntax-splicing

 ;; 12.9 Syntax violations
 syntax-violation)


((rnrs hashtables)
 ;; 13.1 Constructors
 make-eq-hashtable
 make-eqv-hashtable
 make-hashtable

 ;; 13.2 Procedures
 hashtable?
 hashtable-size
 hashtable-ref
 hashtable-set!
 hashtable-delete!
 hashtable-contains?
 hashtable-update!
 hashtable-copy
 hashtable-clear!
 hashtable-keys
 hashtable-entries

 ;; 13.3 Inspection
 hashtable-equivalence-function
 hashtable-hash-function
 hashtable-mutable?

 ;; 13.4 Hash functions
 equal-hash
 string-hash
 string-ci-hash
 symbol-hash)

((rnrs enums)
 ;; 14 Enumerations
 make-enumeration
 enum-set-universe
 enum-set-indexer
 enum-set-constructor
 enum-set->list
 enum-set-member?
 enum-set-subset?
 enum-set=?
 enum-set-union
 enum-set-intersection
 enum-set-difference
 enum-set-complement
 enum-set-projection
 define-enumeration)

((rnrs eval)
 ;; 16 eval
 eval
 environment)

((rnrs mutable-pairs)
 ;; 17 Mutable pairs
 set-car!
 set-cdr!)

((rnrs mutable-strings)
 ;; 18 Mutable strings
 string-set!
 string-fill!)

;; FIXME: R5RS



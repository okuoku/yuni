(library (yuni ffi database types)
         (export
           make-types
           types-add-entry!
           types-entries

           make-type
           type-name
           type-c-enum?
           type-c-enum?-set!
           type-c-struct?
           type-c-struct?-set!
           type-c-union?
           type-c-union?-set!
           type-imported?
           type-imported?-set!
           type-internal?
           type-internal?-set!
           type-c-name
           type-c-name-set!
           type-basetype
           type-pointer-base
           type-pointer-base-set!
           type-array-base-set!
           type-members
           type-members-set!
           type-constraints
           type-add-constraint!

           gen-types-stdc
           gen-types-stdint)
         (import (yuni scheme)
                 (yuni base match)
                 (yuni core)) 

;; types
         
(define* types (typeentries*))

(define (make-types)
  (make types
        (typeentries* '())))

(define* (types-entries (types))
  (~ types 'typeentries*))


(define* type
  (name 
    c-enum?
    c-struct?
    c-union?
    internal?
    imported?
    c-name ;; => string
    type ;; => blob | unsigned-integer | integer | pointer | real 
         ;;    enum-group | flag-group | void | array-pointer
    pointer-base
    members* ;; => symbol*
    constraints* ;; => constraint*
    ))

(define* (types-add-entry! (types) (type))
  (define c (types-entries types))
  (~ types 'typeentries* := (cons type c)))


(define-syntax def
  (syntax-rules ()
    ((_ sym getter setter)
     (begin
       (define* (getter (type))
         (~ type 'sym))
       (define* (setter (type) x)
         (~ type 'sym := x))))))


(define (check-basetype basetype)
  (case basetype
    ((integer real blob pointer enum-group flag-group void unsigned-integer)
     #t)
    (else
      (error "Invalid basetype" basetype))))

(define (make-type basetype name)
  (check-basetype basetype)
  (make type
        (name name)
        (type basetype)
        (c-enum? #f)
        (c-struct? #f)
        (c-union? #f)
        (internal? #f)
        (imported? #f)
        (c-name #f)
        (pointer-base #f)
        (members* '())
        (constraints* '())))

(def name type-name type-name-set!) ;; FIXME: Remove name-set!
(def c-enum? type-c-enum? type-c-enum?-set!)
(def c-struct? type-c-struct? type-c-struct?-set!)
(def c-union? type-c-union? type-c-union?-set!)
(def imported? type-imported? type-imported?-set!)
(def internal? type-internal? type-internal?-set!)
(def c-name type-c-name type-c-name-set!)
(def type type-basetype type-basetype-set!) ;; FIXME: Remove basetype-set!
(def pointer-base type-pointer-base type-pointer-base-set!)
(def members* type-members type-members-set!)

(define* (type-constraints (type))
  (~ type 'constraints*))

(define* (type-add-constraint! (type) const)
  (define c (type-constraints type))
  (~ type 'constraints* := (cons const c)))

(define* (type-array-base-set! (type) base)
  (~ type 'type := 'array-pointer)
  (~ type 'pointer-base := base))

(define stdclib*
  '((integer int)
    (unsigned-integer unsigned-int)
    (integer long)
    (unsigned-integer unsigned-long)
    (integer short)
    (unsigned-integer unsigned-short)
    (integer char)
    (unsigned-integer unsigned-char)

    (float float)
    (float double)

    (void void)))

(define stdintlib*
  '((integer int8_t)
    (unsigned-integer uint8_t)
    (integer int16_t)
    (unsigned-integer uint16_t)
    (integer int32_t)
    (unsigned-integer uint32_t)
    (integer int64_t)
    (unsigned-integer uint64_t)
    ;; FIXME: intmax_t ... here.
    ))

(define (gen-stdlib lis)
  (define out (make-types))
  (define (one obj)
    (match obj
           ((type name)
            (let ((e (make-type type name)))
             (type-imported?-set! e #t)
             (type-internal?-set! e #t)
             (types-add-entry! out e)))))
  (for-each one lis)
  out)

(define (gen-types-stdc) (gen-stdlib stdclib*))
(define (gen-types-stdint) (gen-stdlib stdintlib*))

)

(library (yuni ffi database stubir0)
         (export stubir0->database)
         
         (import (yuni scheme)
                 (yuni ffi database root)
                 (yuni ffi database config)
                 (yuni ffi database libinfo)
                 (yuni ffi database types)
                 (yuni ffi database constraints)
                 (yuni ffi database prologue)
                 (yuni ffi database layouts)
                 (yuni ffi database functions)
                 (yuni ffi database exports)
                 (yuni base match)
                 (yuni core))


;; Common functions

(define (valid-constraint-entry? sym)
  (define xsym (if (pair? sym) (car sym) sym))
  (case xsym
    ((bytelength count constant zero-terminated)
     #t)
    (else #f)))

(define (gen-constraint-entry sym)
  (match sym
         (('count num)
          (make-constraint-count num))
         (('bytelength num)
          (make-constraint-bytelength num))
         (('constant obj)
          (make-constraint-constant obj))
         ('zero-terminated
          (make-constraint-zero-terminated))
         (else
           (error "Invalid constant entry" sym))))

;; Readers

(define (read-exports! db objs)
  (define exports (make-exports))
  (define (do-attr export e)
    (match e
           ('macro
            (export-macro?-set! export #t))))

  (define (one e)
    (match e
           ((type name . attr)
            (let ((export (make-export type name)))
             (for-each (lambda (e) (do-attr export e)) attr)
             (exports-add-entry! exports export)))))
  (for-each one objs)
  (database-exports-set! db exports))

(define (read-functions! db objs)
  (define functions (make-functions))

  (define (attr func arg e)
    ;; FIXME: May receive arg == #f for return-type
    (match e
           ('in
            (argument-input?-set! arg #t))
           ('out
            (argument-output?-set! arg #t))
           (('=> type)
            (argument-type-set! arg type))
           (else
             (let ((c (gen-constraint-entry e)))
              (argument-add-constraint! arg c)))))

  (define (attr-ret func ret e)
    (match e
           ('forward-0
            (function-add-stub-type! func 'forward-0))
           ('forward-1
            (function-add-stub-type! func 'forward-1))
           ('backward-1
            (function-add-stub-type! func 'backward-1))
           ('backward-2
            (function-add-stub-type! func 'backward-2))
           (else (attr func ret e))))

  (define (arg func e)
    (match e
           ((type-spec name . attr*)
            (let ((a (make-argument type-spec name)))
             (for-each (lambda (e) (attr func a e)) attr*)
             (function-add-argument! func a)))))
  (define (one e)
    (match e
           ((return-type-spec name args* . attr*)
            (let* ((rettype 
                     (if (eq? 'void return-type-spec)
                       #f
                       (make-argument return-type-spec #f)))
                   (func (make-function rettype name)))
              (for-each (lambda (e) (arg func e)) args*)
              (for-each (lambda (e) (attr-ret func rettype e)) attr*)
              (functions-add-entry! functions func)))))

  (for-each one objs)
  (database-functions-set! db functions))

(define (read-layouts! db objs)
  (define layouts (make-layouts))
  (define (agr aggregate e)
    ;; FIXME: aggregate may be a procedure... wrong API.
    (define (finish x)
      (cond
        ((procedure? aggregate) (aggregate x))
        (else (aggregate-add-entry! aggregate x))))
    (define (one entry e)
      (match e
             ('array
              (aggregate-entry-array?-set! entry #t))
             (else
               (let ((c (gen-constraint-entry e)))
                (aggregate-entry-add-constraint! entry c)))) )
    (match e
           (('aggregate name . sub*)
            (let* ((lis '())
                   (proc (lambda (ent) (set! lis (cons ent lis)))))
              (for-each (lambda (e) (agr proc e)) sub*)
              (finish (make-aggregate-entry/subaggregate name lis))))
           ((type name . attr*)
            (let ((ent (make-aggregate-entry type name)))
             (for-each (lambda (e) (one ent e)) attr*)
             (finish ent)))))
  (define (one e)
    (match e
           (('aggregate name . entries*)
            (let ((a (make-aggregate name)))
             (for-each (lambda (e) (agr a e)) entries*)
             (layouts-add-aggregate! layouts a)))))
  (for-each one objs)
  (database-layouts-set! db layouts))

(define (read-types! db objs)
  (define types (make-types))

  (define (attr type e)
    (match e
           ('internal 
            (type-internal?-set! type #t))
           ('c-enum
            (type-c-enum?-set! type #t))
           ('c-struct
            (type-c-struct?-set! type #t))
           ('c-union
            (type-c-union?-set! type #t))
           (('pointer-of obj)
            (type-pointer-base-set! type obj))
           (('array-of obj)
            (type-array-base-set! type obj))
           (('members . mem)
            (type-members-set! type mem))
           (else
             (cond
               ((string? e)
                (type-c-name-set! type e))
               ((valid-constraint-entry? e)
                (let ((c (gen-constraint-entry e)))
                 (type-add-constraint! type c)))
               (else
                 (error "Invalid type attr" e))))))
  (define (one e)
    (match e
           ((type name . attr*)
            (let ((t (make-type type name)))
             (for-each (lambda (e) (attr t e))
                       attr*)
             (types-add-entry! types t)))
           (else
             (error "Invalid type entry" e))))

  (for-each one objs)
  (database-types-set! db types))

(define (read-prologue! db objs)
  ;; FIXME: Do we still need symbols* ??
  (define out (make-prologue objs))
  (database-prologue-set! db out))

(define (read-stubir0! db ir)
  (match ir
         (() db)
         (((top . objs) . rest)
          (define (next) (read-stubir0! db rest))
          (case top
            ((config) 'do-nothing (next))
            ((prologue) (read-prologue! db objs) (next))
            ((types) (read-types! db objs) (next))
            ((layouts) (read-layouts! db objs) (next))
            ((functions) (read-functions! db objs) (next))
            ((exports) (read-exports! db objs) (next))
            (else
              (error "Invalid object in ir" top))))))

(define (stubir0->database ir)
  (match ir
         (('stubir0 libname-c libname-scheme . rest)
          (let ((out (make-database)))
           ;; FIXME: Check libname validity here..
           (database-libinfo-set! out (make-libinfo libname-c libname-scheme))
           (read-stubir0! out rest)
           out))
         (else
           (error "Ir is not stubir0" ir))))

)

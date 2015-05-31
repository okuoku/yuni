(library (yuni ffi database flatten)
         (export
           database->flatten/constants
           ;; database-merge-flatten/constants
           )
         (import (yuni scheme)
                 (yuni base match)

                 ;; Database
                 (yuni ffi database root)
                 (yuni ffi database libinfo)
                 (yuni ffi database types)
                 (yuni ffi database layouts)
                 (yuni ffi database exports)
                 
                 ;; FIXME: R6RS libs
                 (only (rnrs) 
                       ;; SRFI-1
                       filter
                       ;; Hashtable
                       make-eq-hashtable
                       hashtable-set!
                       hashtable-ref))

;; 
;; Flatten: convert database into s-exp(lists)
;;
;;
;; Constants DB::
;;    ("db name" (<entry> ...))
;;    
;;    entry ::= 
;;      (constant        "label" <type> value sizeof <if> ...)
;;      (layout          "label" <type> <c-instance-type> sizeof <if> ...)
;;      (aggregate-entry "label" <type> "parent label" "C ref name"
;;                       sizeof offset <if> ...)
;;
;;           type ::= signed | unsigned | real | blob | pointer
;;c-instance-type ::= value | c-struct | c-enum | c-union
;;             if ::= (ifdef "C MACRO symbol")
;;

(define (database->flatten/constants db)
  (define (maybe-null p m) (or (and (not (null? m)) (p m) '())))
  (define libinfo (database-libinfo db))
  (define types (types-entries (database-types db)))
  (define layouts (database-layouts db))
  (define aggregates (layouts-aggregates layouts))
  (define exports (maybe-null exports-entries (database-exports db)))

  ;;; Type resolver
  (define (cache-types!)
    ;; Scan for types and cache type => c-type map
    'do-nothing)
  (define (resolve-type sym)
    ;; Resolve type to c-type(signed / unsigned / real / blob)
    sym)

  ;;; Flatten DB templates
  ;;;; aggregates
  (define (gen-aggregates)
    (define out '())
    (define (push! e) (set! out (cons e out)))
    (define (out-entry top-name cur-name ref-name a)
      (cond
        ((aggregate-entry-has-subaggregate? a)
         (let ((myname (symbol->string (aggregate-entry-name a)))
               (entries (aggregate-entry-subaggregate-entries a)))
           (let ((cur (string-append cur-name "/" myname))
                 (ref (string-append ref-name myname ".")))
             (for-each (lambda (e) (out-entry top-name cur ref e))
                       entries))))
        (else
          (let* ((type (resolve-type (aggregate-entry-type a)))
                 (myname (symbol->string (aggregate-entry-name a)))
                 (name (string-append
                         ref-name
                         myname))
                 (cur (string-append cur-name "/" myname)))
            (push! `(aggregate-entry ,cur ,type ,top-name ,name #f #f))))))
    (for-each (lambda (e)
                (let ((top-name (symbol->string (aggregate-name e))))
                 (for-each (lambda (a)
                             (out-entry top-name top-name "" a))
                           (aggregate-entries e))))
              aggregates)
    out)

  ;;;; layouts
  (define (gen-layouts)
    (define external-types
      (filter (lambda (t) (not (type-internal? t)))
              types))
    (define (gen-entry type)
      (define (gen-instance-type)
        (or (and (type-c-enum? type) 'c-enum)
            (and (type-c-struct? type) 'c-struct)
            (and (type-c-union? type) 'c-union)
            'value))
      (let ((name (symbol->string (type-name type)))
            (instance-type (gen-instance-type))
            (entrytype (resolve-type (type-name type))))
        `(layout ,name ,entrytype ,instance-type #f)))
    (map gen-entry external-types))

  ;;;; constants
  (define (gen-constants/exports)
    (define (gen e)
      (let ((name (symbol->string (export-name e)))
            (type (export-type e))
            (macro? (export-macro? e)))
        `(constant ,name ,(resolve-type type) #f #f
                   ,@(if macro? `((ifdef ,name)) '()))))
    (map gen exports))
  (define (gen-constants/enums)
    (define enum-groups 
      (filter (lambda (e) 
                (eq? 'enum-group 
                     (type-basetype e)))
              types))
    (apply 
      append 
      (map (lambda (e)
             (map (lambda (m)
                    ;; FIXME: Add ifdef ?
                    `(constant ,(symbol->string m)
                               unsigned
                               #f #f)) 
                  (type-members e))) 
           enum-groups)))


  (define (gen-constants)
    (append (gen-constants/exports)
            (gen-constants/enums)))

  ;; Create type cache
  (cache-types!)
  ;; Construct return value
  (let ((myname (symbol->string (libinfo-c-name libinfo)))
        (constants (gen-constants))
        (layouts (gen-layouts))
        (aggregates (gen-aggregates)))
    (list myname (append constants
                         layouts
                         aggregates))))
         
)

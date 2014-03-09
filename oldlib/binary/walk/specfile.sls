(library (yuni binary walk specfile)
         (export parse-spec
                 expand-spec
                 spec-merge-layout
                 )
         (import (rnrs)
                 (match)
                 (srfi :8)
                 (shorten)
                 (yuni core)
                 (yuni binary walk common))

(define (prop-sym-list props)
  (define (q e)
    (match e
           ((sym . obj)
            sym
            )
           (else #f)))
  (filter (^e e) (map q props)))

(define (prop-lookup l props) ;; => sym
  (let ((x (prop-sym-list props)))
    (define (ret m)
      (and (pair? m)
           (or (and (member (car m) x) (car m))
               (ret (cdr m)))))
    (ret l)))

(define (prop-link-type props)
  (prop-lookup '(link-of link-head-of) props))

(define (prop-pickup sym props) ;; => found? value
  (cond
    ((pair? props)
     (if (and (pair? (car props))
              (eq? (caar props) sym))
       (let ((out (car props)))
         (values #t (cdr out)))
       (prop-pickup sym (cdr props))))
    (else (values #f #f))))

(define (gen-link-entry link-type parent name props)
  (define me (make link-entry
                   (parent parent)
                   (name name)
                   (link-head? (eq? 'link-head-of link-type))))
  (receive (found? class) (prop-pickup link-type props)
    (unless found? 
      (assertion-violation 'gen-link-entry
                           "Invalid link-entry"
                           props))
    (~ me 'link-class := (car class)))
  me)

(define (gen-entry constant? parent name props)
  (define (gen)
    (define friends '())
    (define friends-home (if (list? parent) 
                           (append parent (list name))
                           (list parent name)))
    (define me (make entry 
                     (type 'integer)
                     (constant? constant?)
                     (parent parent)
                     (name name)))
    ;(display (list 'parent: parent 'name: name 'props: props))(newline)
    (let-values (((has-count? count) (prop-pickup 'count props))
                 ((has-struct? struct) (prop-pickup 'struct props)))
                ;(display (list 'count: count 'struct: struct))(newline)
      (when count
        ;(display (list 'count: count))(newline)
        ;; FIXME: Integer array?
        (~ me 'type := 'string)
        (~ me 'count := (car count)))
      ;; Generate friends
      (when struct
        (set! friends
          (append friends
                  (map (^e (gen-entry #f friends-home (car e) (cdr e)))
                       struct)))
        ;(write (list 'friends: friends))(newline)
        ))
    (cons me friends))
  (let ((link-type (prop-link-type props)))
    (if link-type
      (gen-link-entry link-type parent name props)
      (gen))))

(define (flatten entries)
  ;; FIXME: Braindamaged
  (define out '())
  (define (push x) (set! out (cons x out)))
  (define (itr x)
    (cond
      ((pair? x)
       (itr (car x)) 
       (itr (cdr x)))
      ((null? x)
       ;; Ignore
       'ok)
      (else
        (push x))))
  (itr entries)
  out)

(define (gen-struct mytype name props)
  (define me (make type 
                   (name name)
                   (type mytype)))
  (define (gen x)
    ;(display (list 'gen: x))(newline)
    (match x
           ((entry-name . props)
            (gen-entry #f name entry-name props))))
  (let ((entries (map gen props)))
    (~ me 'entries* := entries)
    (cons me (flatten entries))))

(define (gen-network name props)
  (define me (make network
                   (name name)
                   (type 'list)))
  (define (fill x)
    (match x
           (('head-first name)
            (~ me 'head-first := name))
           (('next name)
            (~ me 'next := name))
           (('prev name)
            (~ me 'prev := name))))
  (for-each fill props)
  ;; gen-* should return a list
  (list me))

(define (conv-spec e) ;; => (network/entry ...) / '()
  (match e
         (('global name . props)
          (gen-entry #f #t name props))
         (('constant name . props)
          (gen-entry #t #t name props))
         (('struct name . props)
          (gen-struct 'struct name props))
         (('c-struct name . props)
          (gen-struct 'c-struct name props))
         (('network name . props)
          (gen-network name props))
         (else '())))

(define (parse-spec l) ;; => (obj ...)
  (define (gen x)
    (let ((r (conv-spec x)))
      (if (pair? r) r (list r))))

  (apply append (map gen l)))

(define (expand-spec l)
  ;; Expand spec
  ;; FIXME: Implement it...
  l)

(define (entry-sym e)
  (let-with e (parent name)
    (cond
      ((list? parent)
       (sym-cstruct (car parent) (append (cdr parent) (list name)))) 
      ((symbol? parent)
        (sym-cstruct parent (list name))) 
      (else name))))

(define (spec-merge-layout l layout)
  (define layout-ht (make-eq-hashtable))
  (define (merge-layout e)
    (cond
      ((is-a? e entry)
       (let ((p (hashtable-ref layout-ht (entry-sym e) #f)))
         ;(display (list 'fill: (entry-sym e) p))(newline)
         (when p
           (~ e 'size := (cdr p))
           (cond
             ((~ e 'constant?)
              (~ e 'value := (car p)))
             (else
               (~ e 'offset := (car p)))))))))
  ;; Construct layout-ht
  (for-each (^m (match m ((name value size)
                          (hashtable-set! layout-ht name (cons value size)))))
            layout)
  ;; Merge each entry
  (for-each merge-layout l))

)

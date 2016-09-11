(library (yuni ffi runtime yunistub-procs-layout)
         (export
           make-layout-proc
           make-field-proc
           ref?
           make-ref
           size-of
           offset-of)
         (import (yuni scheme)
                 (yuni core)
                 (yuni miniobj minidispatch)
                 (yuni base match)
                 (yuni base dispatch))

         

;; Make-reference (re-exported as (yuni ffi))

(define* <reference> (layout obj offset deferred*))

(define make-ref
  (case-lambda
    ((layout obj)
     (make-ref layout obj 0))
    ((layout obj offset)
     (make <reference>
           (layout layout)
           (obj obj)
           (offset offset)
           (deferred* '()))))) 

(define (ref? obj)
  (is-a? obj <reference>))

(define* (ref-layout (obj <reference>))
  (~ obj 'layout))

; private

(define* (ref-add-offset (obj <reference>) add-offset)
  (let-with obj (layout obj offset deferred*)
    (make <reference>
          (layout layout)
          (obj obj)
          (offset (+ offset add-offset))
          (deferred* '()))))

;; Offset/Size query (re-exported as (yuni ffi))
(define (do-query/single sym layout)
  (define aux (miniobj-minidispatch-aux layout))
  (aux sym))

(define (do-query/multiple sym layout . member-ref*)
  (if (pair? member-ref*)
    (let ((next (cdr member-ref*))
          (p (~ layout (car member-ref*))))
      (apply do-query/multiple sym p next))
    (do-query/single sym layout)))

(define (query-target->layout obj)
  (cond
    ((ref? obj) (ref-layout obj))
    (else obj)))

(define size-of
  (case-lambda
    ((layout?)
     (do-query/single 'sizeof layout?))
    ((layout? . member-ref*)
     (apply do-query/multiple 'sizeof (query-target->layout layout?) 
            member-ref*))))

(define offset-of
  (case-lambda
    ((layout?)
     (do-query/single 'offsetof layout?))
    ((layout? . member-ref*)
     (apply do-query/multiple 'offsetof (query-target->layout layout?)
            member-ref*))))

;; procs (for (yuni ffi runtime yunistub-layout))

(define (do-ref me offset size type)
  (error "Implement me"))

(define (do-set! me offset size type obj)
  (error "Implement me"))

(define (make-field-proc sym param child)
  (define-minidispatch-class sublayout self)
  (define offset (if param (car param) 0))
  (define size (if param (cadr param) 0))
  (define type (if param (cadr (cdr param)) 'none))
  (define childclass (and child (make-minidispatch-obj child #f)))
  (define (reffunc slot me)
    ;; slot = #f for self reference
    (cond
      ((and me (not child)) ;; I'm reference object and terminator
        (do-ref me offset size type))
      (else ;; Return reference object
        (cond
          (slot ;; child reference
            (unless child
              (error "no next member" slot))
            (cond
              (me
                (make-minidispatch-obj sublayout
                                       (ref-add-offset me offset)))
              (else
                (~ childclass slot))))
          (else
            ;; Build layout object of myself
            (make-minidispatch-obj sublayout #f))))))
  (define (set!func slot me obj)
    (cond
      (me ;; I'm reference object
        (do-set! me offset size type obj))
      (else ;; Layout object. Cannot set.
        (error "Layout object is immutable" sym))))
  (define self
    (dispatch-lambda
      ;; From make-layout-func
      (('ref slot me itsmyself)
       (unless (eq? itsmyself 'itsmyself)
         (error "something wrong"))
       ;; Self call
       (reffunc #f me))
      (('ref slot me)
       ;; Call from parent
       (reffunc slot me))
      (('set! slot me obj)
       (set!func slot me obj))
      ;; AUX call
      (('sizeof)
       size)
      (('offsetof)
       offset)))
  self)

(define (make-layout-proc param dispatch)
  (define offset (and param (car param)))
  (define size (and param (cadr param)))
  (define type (and param (cadr (cdr param))))
  (dispatch-lambda
    ;; Generic
    (('ref slot me)
     (dispatch slot 'ref slot me 'itsmyself))
    (('set! slot me obj)
     (dispatch slot 'set! slot me obj))
    ;; Class requests
    (('sizeof)
     size)
    (('offsetof)
     offset)))

)

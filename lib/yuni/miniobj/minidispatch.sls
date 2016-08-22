(library (yuni miniobj minidispatch)
         (export
           miniobj-minidispatch-ref
           miniobj-minidispatch-set!
           miniobj-minidispatch-typeof
           miniobj-minidispatch-aux
           define-minidispatch-class
           make-minidispatch-obj)
         (import (yuni scheme)
                 (yuni miniobj minitype))

         
;;

(define-minitype <minidispatch>
                 (name func))

(define-minitype <minidispatch-obj>
                 (minitype obj))

(define (baseref obj slot)
  (define (complain obj slot)
    (error "Fatal error" obj slot))
  (miniobj-minitype-ref obj slot complain))
(define (baseset! obj slot v)
  (define (complain obj slot v)
    (error "Fatal error" obj slot))
  (miniobj-minitype-set! obj slot v complain))

(define (minidispatch-obj? obj)
  (minitype-predicate obj <minidispatch-obj>))

(define (minidispatch-class? obj)
  (minitype-predicate obj <minidispatch>))

(define-syntax define-minidispatch-class
  (syntax-rules ()
    ((_ nam fun)
     (define nam
       (let ((obj ((make-minitype-obj <minidispatch>))))
        (baseset! obj 'name 'nam)
        (baseset! obj 'func fun)
        obj)))))

(define (make-minidispatch-obj class param)
  (unless (minidispatch-class? class)
    (error "minidispatch class required" class))
  (let ((obj ((make-minitype-obj <minidispatch-obj>))))
   (baseset! obj 'minitype class)
   (baseset! obj 'obj param)
   obj))

(define (do-ref obj slot)
  (let* ((typ (baseref obj 'minitype))
         (func (baseref typ 'func))
         (o (baseref obj 'obj)))
    (func 'ref slot o)))

(define (do-set! obj slot value)
  (let* ((typ (baseref obj 'minitype))
         (func (baseref typ 'func))
         (o (baseref obj 'obj)))
    (func 'set! slot o value)))

(define (do-typeof obj)
  (define typ (baseref obj 'minitype))
  typ)

(define (miniobj-minidispatch-aux obj)
  (let* ((typ (baseref obj 'minitype))
         (func (baseref typ 'func)))
    func))

(define (miniobj-minidispatch-ref obj slot k)
  (if (minidispatch-obj? obj)
    (do-ref obj slot)
    (k obj slot))) 

(define (miniobj-minidispatch-set! obj slot value k)
  (if (minidispatch-obj? obj)
    (do-set! obj slot value)
    (k obj slot value)))

(define (miniobj-minidispatch-typeof obj k)
  (if (minidispatch-obj? obj)
    (do-typeof obj)
    (k obj)))
)

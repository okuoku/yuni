(library (yuni minife identifiers)
         (export
           id?
           id-new-variable
           id-new-macro

           id-new-library-variable
           id-new-library-macro
           id-new-primitive
           id-new-aux-syntax

           id-set-global-name!
           id-set-gensym-ident!
           id-set-library!
           
           id-source-name
           id-global-name
           id-gensym-ident
           id-library
           id-variable?
           id-macro?
           id-aux-syntax?
           id-primitive?)
         (import (yuni scheme))

;; id = #(id source-name global-name gensym-ident library type primitive?)

(define id-ident (cons 'id '()))

(define (id? obj) (and (vector? obj)
                       (>= (vector-length obj) 1)
                       (eq? id-ident (vector-ref obj 0))))

(define (id-source-name id) (vector-ref id 1))
(define (id-global-name id) (vector-ref id 2))
(define (id-gensym-ident id) (vector-ref id 3))
(define (id-library id) (vector-ref id 4))
(define (%id-type id) (vector-ref id 5))
(define (id-variable? id) (eq? (%id-type id) 'variable))
(define (id-macro? id) (eq? (%id-type id) 'macro))
(define (id-aux-syntax? id) (eq? (%id-type id) 'aux-syntax))
(define (id-primitive? id) (vector-ref id 6))

(define (id-set-global-name! id name) (vector-set! id 2 name))
(define (id-set-gensym-ident! id ident) (vector-set! id 3 ident))
(define (id-set-library! id lib) (vector-set! id 4 lib))

(define (id-new-variable sym)
  (vector id-ident sym sym 0 #t 'variable #f))
(define (id-new-macro sym)
  (vector id-ident sym sym 0 #t 'macro #f))

(define (id-new-library-variable sym global lib)
  (vector id-ident sym global 0 lib 'variable #f))
(define (id-new-library-macro sym global lib)
  (vector id-ident sym global 0 lib 'macro #f))
(define (id-new-primitive sym global lib)
  (vector id-ident sym global 0 lib 'variable #t))
(define (id-new-aux-syntax sym lib)
  (vector id-ident sym sym 0 lib 'aux-syntax #f))


)

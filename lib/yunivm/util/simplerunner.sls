(library (yunivm util simplerunner)
         (export 
           simplerunner/treeir-compile
           simplerunner/treeir-run
           new-simplerunner)
         (import (yuni scheme)
                 (yunivm compiler compilercore)
                 (yunivm vm seq-treeir))

         
;; Quickrunner for test
         
(define-syntax gen-basiclibs
  (syntax-rules ()
    ((_ frm ...)
     (list (cons 'frm (seq-treeir-make-primitive frm)) ...))))

(define basiclibs
  (gen-basiclibs
    ;; FIXME: Export all (scheme base) procs.
    +
    list
    cons
    car
    cdr
    display
    newline
    for-each
    write
    reverse
    null?
    equal?
    list->vector
    vector-ref))

(define vec-global0
  (list->vector
    (map car basiclibs)))

(define vec-global1
  (list->vector
    (map cdr basiclibs)))

(define (simplerunner/treeir-compile runner frm)
  (call-with-values (lambda () (compile-core frm vec-global0))
                    (lambda (ir mx) ir)))

(define (simplerunner/treeir-run runner ir)
  (define (global mod no)
    (unless (= mod 0)
      (error "Something wrong" mod no))
    (vector-ref vec-global1 no))
  (seq-treeir global ir))

(define (new-simplerunner) #f)

)

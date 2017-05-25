(library (yunivm util simplerunner)
         (export 
           simplerunner/treeir-compile
           simplerunner/treeir-run
           new-simplerunner)
         (import (yuni scheme)
                 (yunivm compiler compilercore)
                 (yunivm vm seq-treeir)
                 (yunivm util basiclibs)
                 (yunivm loader generator))

         
;; Quickrunner for test

(define basiclibs-proc-vector-converted
  (list->vector
    (map (lambda (p) (seq-treeir-make-primitive p))
         (vector->list basiclibs-proc-vector))))

(define (simplerunner/treeir-compile runner frm)
  (call-with-values (lambda () (compile-core frm basiclibs-name-vector))
                    (lambda (ir mx) ir)))

(define (simplerunner/treeir-run runner ir)
  (define (global mod no)
    (unless (= mod 0)
      (error "Something wrong" mod no))
    (vector-ref basiclibs-proc-vector-converted no))
  (seq-treeir global ir))

(define (new-simplerunner) #f)

)

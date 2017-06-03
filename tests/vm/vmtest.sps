(import (yuni scheme)
        (yunivm expander expandcore)
        (yunivm heap pass)
        (yunivm vm seq-treeir))

(define test-counter 0)
(define success-counter 0)
(define failed-forms '())

(define (check-finish)
  (display "Test: ")
  (display success-counter)
  (display "/")
  (display test-counter)
  (display " passed.") (newline)
  (unless (null? failed-forms)
    (newline)
    (display "Failed: ")
    (newline)
    (for-each (lambda x
                (display "    ")
                (write x)
                (newline))
              (reverse failed-forms)))
  (flush-output-port (current-output-port))
  (exit (if (null? failed-forms) 0 1)))


(define-syntax check-equal
  (syntax-rules ()
    ((_ obj form)
     (begin
       (set! test-counter (+ 1 test-counter))
       (let ((e form))
        (cond ((equal? obj e)
               (set! success-counter (+ 1 success-counter)))
              (else
                (set! failed-forms (cons 'form failed-forms)))))))))

(define-syntax check-treeir
  (syntax-rules ()
    ((_ (global-variables ...) IR Expected)
     (let* ((heap (make-heap-pass (list global-variables ...))))
       (set! test-counter (+ 1 test-counter))
       (call-with-values (lambda () (seq-treeir heap IR))
                         (lambda vals
                           (cond
                             ((equal? vals 'Expected)
                              (set! success-counter (+ 1 success-counter)))
                             (else
                               (set! failed-forms (cons
                                                    (list
                                                      (cons 'IR 'IR)
                                                      (cons 'Exp 'Expected)
                                                      (cons 'Actual vals))
                                                    failed-forms))))))))))

(check-treeir
  () ;; global-variables
  '() ;; TreeIR
  () ;; expected values
  )

(check-treeir
  ;; global-variables
  () 
  ;; TreeIR
  '((FRAME 0)) 
  ;; expected values
  ())

(check-treeir
  ;; global-variables
  () 
  ;; TreeIR
  '((LDI 123)) 
  ;; expected values
  (123))

(check-treeir
  ;; global-variables
  () 
  ;; TreeIR
  '((FRAME 3)
    (LDI 123)
    (MOV 0)
    (LDI 1)
    (MOV 1)
    (LDI 2)
    (MOV 2)
    (LDV))
  ;; expected values
  (123 1 2))

(check-treeir
  ()
  '((FRAME 1)
    (LDI 123)
    (MOV 0)
    (BIND)
    (FRAME 1)
    (LDI 456)
    (ST 0 0)
    (LDI 987)
    (LD 0 0))
  (456))

(check-treeir
  ()
  '((FRAME 1)
    (LDI 123)
    (MOV 0)
    (LDV))
  (123))

(check-treeir
  ()
  '((FRAME 3)
    (LDI 1)
    (MOV 0)
    (LDI 2)
    (MOV 1)
    (LDI 3)
    (MOV 2)
    (BIND)
    (FRAME 3)
    (LDI 11)
    (MOV 0)
    (LDI 12)
    (MOV 1)
    (LDI 13)
    (MOV 2)
    (BIND)
    (FRAME 6)
    (LD 0 0)
    (MOV 0)
    (LD 0 1)
    (MOV 1)
    (LD 0 2)
    (MOV 2)
    (LD 1 0)
    (MOV 3)
    (LD 1 1)
    (MOV 4)
    (LD 1 2)
    (MOV 5)
    (LDV)
    (LEAVE)
    (LEAVE))
  (11 12 13 1 2 3))

(check-treeir
  (123)
  '((LDG 0 0))
  (123))

(check-treeir
  ()
  '((LDC 124))
  (124))

(check-treeir
  ((lambda (arg) 
     (check-equal arg 123) 1))
  '((FRAME 1)
    (LDI 123)
    (MOV 0)
    (LDG 0 0)
    (CALL))
  (1))

(check-treeir
  ((lambda () 1))
  '((FRAME 0)
    (LDG 0 0)
    (CALL))
  (1))

(check-treeir
  ((lambda () (values)))
  '((FRAME 0)
    (LDG 0 0)
    (CALL))
  ())

(check-treeir
  ((lambda () (values 123 456)))
  '((FRAME 0)
    (LDG 0 0)
    (CALL))
  (123 456))

(check-treeir
  ((lambda (a b . c)
     (check-equal a 123)
     (check-equal b 1)
     (check-equal c '(2 3))
     1))
  '((FRAME 4)
    (LDI 123)
    (MOV 0)
    (LDI 1)
    (MOV 1)
    (LDI 2)
    (MOV 2)
    (LDI 3)
    (MOV 3)
    (LDG 0 0)
    (CALL))
  (1))

(check-treeir
  ((lambda () #t))
  '((block 0
           (FRAME 0)
           (LDG 0 0)
           (CALL)
           (BRV (enter 1))
           (JMP (break 0))
           (block 1
                  (FRAME 2)
                  (LDC 123)
                  (MOV 0)
                  (LDC 456)
                  (MOV 1)
                  (LDV))))
  (123 456))

(define truefalse-123-456
  '((block 0
           (FRAME 0)
           (LDG 0 0)
           (CALL)
           (BRV (enter 1))
           (LDI 456)
           (JMP (break 0))
           (block 1
                  (LDI 123)))))

(check-treeir
  ((lambda () #t))
  truefalse-123-456
  (123))

(check-treeir
  ((lambda () #f))
  truefalse-123-456
  (456))

(check-treeir
  ()
  '((FRAME 2)
    (LDI 123)
    (MOV 0)
    (BIND)
    (LDF (enter 1))
    (block 0
           (LEAVE)
           (FRAME 1)
           (BIND)
           (FRAME 1)
           (BIND)
           (FRAME 1)
           (MOV 0)
           (BIND)
           (FRAME 1)
           (LDI 456)
           (MOV 0)
           (LD 0 0)
           (CALL)
           (JMP (break 0))
           (block 1
                  (RECV 1)
                  (FRAME 2)
                  (LD 1 0)
                  (MOV 0)
                  (LD 0 0)
                  (MOV 1)
                  (LDV)
                  (RET))))
  (123 456))

(check-treeir
  ((lambda (a) (check-equal a 123) 456))
  '((FRAME 1)
    (LDG 0 0)
    (MOV 0)
    (BIND)
    (LDF (enter 1))
    (block 0
           (LEAVE)
           (FRAME 0)
           (CALL)
           (JMP (break 0))
           (block 1
                  (RECV 0)
                  (FRAME 1)
                  (LDI 123)
                  (MOV 0)
                  (LD 1 0)
                  (TCALL)))) 
  (456))

(check-treeir
  ((lambda () (list 123 456 789)))
  '((FRAME 1)
    (FRAME 0)
    (LDG 0 0)
    (CALL)
    (MOV 0)
    (LDF (enter 1))
    (CALLM)
    (block 0
           (JMP (break 0))
           (block 1
                  ;; (lambda (a b c) (values c b a))
                  (RECV 3)
                  (FRAME 3)
                  (LD 0 0)
                  (MOV 2)
                  (LD 0 1)
                  (MOV 1)
                  (LD 0 2)
                  (MOV 0)
                  (LDV)
                  (RET))))
  (789 456 123))


(check-treeir
  ((lambda () (list 123 456 789)))
  '((FRAME 1)
    (FRAME 0)
    (LDG 0 0)
    (CALL)
    (MOV 0)
    (LDF (enter 1))
    (CALLM)
    (block 0
           (JMP (break 0))
           (block 1
                  ;; (lambda (a b c . d) (values d c))
                  (RECVM 3)
                  (FRAME 2)
                  (LD 0 3)
                  (MOV 0)
                  (LD 0 2)
                  (MOV 1)
                  (LDV)
                  (RET))))
  (() 789))

(check-treeir
  ;; Fast-path case
  ((lambda () (list 123 456 789)))
  '((FRAME 3)
    (FRAME 0)
    (LDG 0 0)
    (CALL)
    (MOV 2)
    (LDI 1)
    (MOV 0)
    (LDI 2)
    (MOV 1)
    (LDF (enter 1))
    ;; (apply proc 1 2 '(123 456 789))
    (CALLM)
    (block 0
           (JMP (break 0))
           (block 1
                  ;; (lambda (a b . c) (values a c))
                  (RECVM 2)
                  (FRAME 2)
                  (LD 0 0)
                  (MOV 0)
                  (LD 0 2)
                  (MOV 1)
                  (LDV)
                  (RET))))
  (1 (123 456 789)))

(check-treeir
  ;; Slow-path case
  ((lambda () (list 123 456 789)))
  '((FRAME 3)
    (FRAME 0)
    (LDG 0 0)
    (CALL)
    (MOV 2)
    (LDI 1)
    (MOV 0)
    (LDI 2)
    (MOV 1)
    (LDF (enter 1))
    ;; (apply proc 1 2 '(123 456 789))
    (CALLM)
    (block 0
           (JMP (break 0))
           (block 1
                  ;; (lambda (a . b) (values a b))
                  (RECVM 1)
                  (FRAME 2)
                  (LD 0 0)
                  (MOV 0)
                  (LD 0 1)
                  (MOV 1)
                  (LDV)
                  (RET))))
  (1 (2 123 456 789)))

(check-treeir
  ()
  '((FRAME 1) 
    (BIND) 
    (block 0 
           (LDF (enter 1)) 
           (JMP (break 0)) 
           (block 1 
                  (RECVM 2) 
                  (LD 0 2) 
                  (RET))) 
    (ST 0 0) 
    (FRAME 3) 
    (LDI 1) 
    (MOV 0) 
    (LDI 2) 
    (MOV 1) 
    (LDI 3) 
    (MOV 2) 
    (LD 0 0) 
    (CALL) 
    (LEAVE))
  ((3))
  )

(display "Corelib-null:\n")

(write (get-core-library0))

(newline)

(display "Expand-null:\n")

(write (expand0 '((begin (let ((a 1)) a)))))

(newline)

(check-finish)

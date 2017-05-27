(library (yunivm expander corelangfilter)
         (export corelangfilter)
         (import (yuni scheme))

         
;; 
;; Filter core scheme output
;;
;; (define xxxx #f)
;; (define yyyy #f)
;; ...
;; (let ()
;;   (set! xxxx XXXX)
;;   (set! yyyy YYYY)
;;   ...)
;; (define XXXX xxxx) ;; Eliminate this feedback-defines
;; (define YYYY yyyy)
;;   
;;   =>
;; 
;; (global-define xxxx XXXX)
;; (global-define yyyy YYYY)
;;

(define (corelangfilter seq) 
  (define global-symbol-candidates '())
  (define global-setup-candidates '())
  (define global-defines '())
  (define output-body '())

  ;; Commands
  (define (add-body! e) (set! output-body (cons e output-body)))
  (define (add-setup-candidate! sym content)
    (set! global-setup-candidates (cons (cons sym content)
                                        global-setup-candidates)))
  (define (add-sym-candidate! sym) (set! global-symbol-candidates
                                     (cons sym global-symbol-candidates)))

  (define (seemingly-global-symbol? sym)
    (let loop ((rest global-symbol-candidates))
     (and (pair? rest)
          (let ((qsym (car rest))
                (next (cdr rest)))
            (or (eq? qsym sym)
                (loop next))))))

  (define (global-symbol? sym)
    (let loop ((rest global-setup-candidates))
     (and (pair? rest)
          (let ((qsym (caar rest))
                (next (cdr rest)))
            (or (eq? qsym sym)
                (loop next))))))

  ;; Step1 scan sequence
  (let ()
   (define (scan-setup-let e)
     (when (and (pair? e) (eq? 'set! (car e)))
       (let ((sym (cadr e))
             (content (caddr e)))
         (when (seemingly-global-symbol? sym)
           (add-setup-candidate! sym content)))))
   (define (step1 e)
     (cond
       ((pair? e)
        (let ((head (car e)))
         (case head
           ((define)
            (let ((a (cadr e))
                  (b (caddr e)))
              (when (eq? #f b)
                (add-sym-candidate! a))))
           ((let)
            (when (null? (cadr e))
              (for-each scan-setup-let (cddr e))))
           ((begin)
            (for-each step1 (cdr e))))))))
   (for-each step1 seq))

  ;; Step2 filter-out global feedbacks
  (let ()
   (define (filter-setup-let2 seq)
     (or (and (pair? seq) (pair? (car seq)) (eq? 'set! (caar seq))
              (global-symbol? (cadar seq))
              (filter-setup-let2 (cdr seq)))
         seq))
   (define (step2 e)
     (cond
       ((pair? e)
        (let ((head (car e)))
         (case head
           ((define)
            (let ((a (cadr e))
                  (b (caddr e)))
              (cond
                ((eq? b #f)
                 (unless (global-symbol? a)
                   (add-body! e)))
                (else
                  (unless (global-symbol? b)
                    (add-body! e))))))
           ((let)
            (cond
              ((null? (cadr e))
               (let ((body (filter-setup-let2 (cddr e))))
                (unless (null? body)
                  (add-body! (cons 'let
                                   (cons '()
                                         body))))))
              (else
                (add-body! e))))
           ((begin)
            (for-each step2 (cdr e)))
           (else
             (add-body! e)))))))
   (for-each step2 seq))

  ;; Output
  (append (map (lambda (e)
                 (let ((sym (car e))
                       (setup (cdr e)))
                   (list 'global-define sym setup)))
               (reverse global-setup-candidates))
          (reverse output-body)))

)

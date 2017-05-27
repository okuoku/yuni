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
  (define macro-imports '())
  (define top-level-defines '())
  (define output-body '())

  ;; Commands
  (define (add-setup-candidate! sym content)
    (set! global-setup-candidates (cons (cons sym content)
                                        global-setup-candidates)))
  (define (add-sym-candidate! sym) (set! global-symbol-candidates
                                     (cons sym global-symbol-candidates)))
  (define (add-top-level-definition! frm) (set! top-level-defines
                                            (cons frm top-level-defines)))
  (define (add-macro-import! sym gsym)
    (let ((p (assoc sym macro-imports)))
     (unless p
       (set! macro-imports
         (cons (cons sym gsym)
               macro-imports)))))

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

  (define (macro-import-name sym)
    (let ((p (assoc sym macro-imports)))
     (and p (cdr p))))

  (define (quote-key? frm key)
    (and (pair? (cadr frm))
         (eq? key (caadr frm))))

  (define (quote-key-content frm) (cdadr frm))

  ;; Step1 scan sequence
  (let ()
   (define (scan-setup-let e)
     (when (and (pair? e) (eq? 'set! (car e)))
       (let ((sym (cadr e))
             (content (caddr e)))
         (when (seemingly-global-symbol? sym)
           (add-setup-candidate! sym content)))))
   (define (step1/toplevel e) (step1 'toplevel e))
   (define (step1/subforms e) (step1 'subforms e))
   (define (step1/libtop e)   (step1 'libtop e))
   (define (step1 context e)
     (cond
       ((pair? e)
        (let ((head (car e)))
         (case head
           ((quote)
            (when (quote-key? e '*yunifake-renames*)
              (for-each (lambda (e)
                          (add-macro-import! (car e) (cdr e)))
                        (quote-key-content e))))
           ((define)
            (let ((a (cadr e))
                  (b (caddr e)))
              (when (eq? #f b)
                (add-sym-candidate! a))))
           ((let)
            (when (null? (cadr e))
              (for-each scan-setup-let (cddr e))))
           ((begin)
            (case context
              ((toplevel)
               (for-each step1/libtop (cdr e)))
              ((subforms libtop)
               (for-each step1/subforms (cdr e))))))))))
   (for-each step1/toplevel seq))

  ;; Step2 filter-out global feedbacks and explode defines
  (let ()
  (define (add-body! e) 
    (cond
      (has-macro-export?
        (set! deferred-body (cons e deferred-body)))
      (else
        (set! output-body (cons e output-body)))))
   (define deferred-symbols '())
   (define deferred-body '())
   (define has-macro-export? #f)
   (define (end-library!)
     (set! has-macro-export? #f)
     (for-each (lambda (e)
                 (add-body! (list 'set! (car e) (cdr e))))
               deferred-symbols)
     (set! deferred-symbols '())
     (for-each add-body!  (reverse deferred-body))
     (set! deferred-body '()))
   (define (add-deferred-symbol! to from)
     (set! deferred-symbols (cons (cons to from) deferred-symbols)))
   (define (filter-setup-let2 seq)
     (or (and (pair? seq) (pair? (car seq)) (eq? 'set! (caar seq))
              (global-symbol? (cadar seq))
              (filter-setup-let2 (cdr seq)))
         seq))
   (define (step2/toplevel e) (step2 'toplevel e))
   (define (step2/subforms e) (step2 'subforms e))
   (define (step2/libtop e)   (step2 'libtop e))
   (define (step2 context e)
     (cond
       ((pair? e)
        (let ((head (car e)))
         (case head
           ((quote)
            (cond
              ((quote-key? e '*yunifake-renames*)
               (set! has-macro-export? #t))
              ((quote-key? e '*yunifake-exports*)
               (for-each (lambda (sym)
                           (let ((nam (macro-import-name sym)))
                            (when nam
                              (add-deferred-symbol! sym nam))))
                         (quote-key-content e)))
              ((quote-key? e '*yunifake-libend*)
               (end-library!))
              (else
                (add-body! e))))
           ((define)
            (let ((a (cadr e))
                  (b (caddr e)))
              (cond
                ((eq? b #f)
                 (unless (global-symbol? a)
                   (add-top-level-definition! e)))
                (else
                  (unless (global-symbol? b)
                    (cond
                      (has-macro-export?
                        (add-top-level-definition! (list 'define a #f))
                        (add-deferred-symbol! a b))
                      (else
                        (add-top-level-definition! e))))))))
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
            (case context
              ((toplevel)
               (for-each step2/libtop (cdr e)))
              ((subforms libtop)
               (for-each step2/subforms (cdr e)))))
           (else
             (add-body! e)))))
       (else
         (add-body! e))))
   (for-each step2/toplevel seq))

  ;; Output
  (append (map (lambda (e)
                 (let ((sym (car e))
                       (setup (cdr e)))
                   (list 'global-define sym setup)))
               (reverse global-setup-candidates))
          (reverse top-level-defines)
          (reverse output-body)))

)

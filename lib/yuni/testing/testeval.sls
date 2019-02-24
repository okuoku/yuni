(library (yuni testing testeval)
         (export testeval
                 failure?
                 failure->string
                 ;; failure-library-not-found?
                 ;; failure-library
                 ;; failure-symbol-not-found?
                 ;; failure-read-datum?
                 )
         (import (yuni scheme)
                 (yuni compat eval))

(define failure-header (list "Yuni testing failure"))

(define (failure? x) (not (eq? #t x)))

(define (failure->string f)
  (let ((p (open-output-string)))
   (write f p)
   (get-output-string p)))

(define (testeval form lib*) ;; => result failure?
  (define (libname lib)
    (if (pair? lib)
      (case (car lib)
        ((rename except only)
         (libname (cadr lib)))
        (else lib))))
  (define (assert-for-library lib) ;; => failure? / #f
    (guard (c (#t 
               ;; FIXME: Generate library error object here.
               c))
           (begin
             (eval 123 (environment lib))
             #f)))
  (define (assert-for-libraries libs*) ;; => failure? / #f for success
    (and (pair? libs*)
         (or (assert-for-library (libname (car libs*)))
             (assert-for-libraries (cdr libs*)))))
  ;; First, query for every libraries
  (let ((libresult (assert-for-libraries lib*)))
   (if libresult
     (values #f libresult)
     ;; Then, evaluate actual code
     (guard (c (#t (values #f c)))
            (let ((ret (eval form (apply environment lib*))))
             (values ret #t))))))
         
)

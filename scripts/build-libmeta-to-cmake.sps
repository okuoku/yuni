(import (scheme base)
        (scheme cxr)
        (scheme file)
        (scheme write)
        (scheme read)
        (scheme process-context))

(define (file->list proc pth)
  (write pth) (newline)
  (call-with-input-file
    pth
    (lambda (p)
      (define (itr cur)
        (let ((r (proc p)))
          (if (eof-object? r)
            (reverse cur)
            (itr (cons r cur)))))
      (itr '()))))
 
(define (file->sexp-list pth)
  (file->list read pth))
 
(define (calc-input-file)
  (define (usage)
    (display "Usage: build-libmeta-to-cmake.sps <libfile.sls> <out.cmake>\n")
    (exit #f))
  (let ((l (command-line)))
   (unless (= 3 (length l)) (usage))
   (let ((fn (cadr l)))
    (unless (file-exists? fn)
      (usage))
    fn)))

(define (calc-output-file)
  (let ((l (command-line)))
   (let ((fn (caddr l)))
    fn)))

(define (libspec->string spec)
  (define (lis->string acc spec)
    (define (enter x)
      (cond
        ((pair? x)
         (libspec->string x))
        (else (symbol->string x))))
    (if (pair? spec)
      (lis->string
        (if (string=? "" acc)
          (enter (car spec))
          (string-append acc " " (enter (car spec))))
        (cdr spec))
      acc))
  (if (pair? spec)
    (string-append "("
                   (lis->string "" spec)
                   ")")
    (symbol->string spec)))

(define (libname->cmakesym spec)
  (define (itr acc cur)
    (if (pair? cur)
      (itr (string-append acc "_" (symbol->string (car cur)))
           (cdr cur))
      acc))
  (itr (symbol->string (car spec)) (cdr spec)))

;; Output utils 
(define (say p . x)
  (for-each (lambda (x) 
              (if (list? x)
                (apply say p x)
                (display x p)))
            x)) 

(define (quotstr x)
  (string-append "\"" x "\" "))

;; Main
(let* ((in (calc-input-file))
       (out (calc-output-file)))
  (let ((code (file->sexp-list in)))
   (unless (and (pair? code) (list? code) (= 1 (length code)))
     (error "Invalid library format" in code))
   (let* ((lib (car code))
          (head (car lib))
          (name (cadr lib))
          (exports (cdr (caddr lib)))
          (imports (cdr (cadddr lib))))
     (when (file-exists? out)
       (delete-file out))
     (call-with-output-file
       out
       (lambda (p)
         (let ((libsym (libname->cmakesym name)))
          ;; Exports
          (say p "set(libexports_" libsym "\n")
          (for-each (lambda (e)
                      (say p "  " 
                           (quotstr (libspec->string e))
                           "\n"))
                    exports)
          (say p ")\n")
          ;; Imports
          (say p "set(libimports_" libsym "\n")
          (for-each (lambda (e)
                      (say p "  " 
                           (quotstr (libspec->string e))
                           "\n"))
                    imports)
          (say p ")\n")))))))

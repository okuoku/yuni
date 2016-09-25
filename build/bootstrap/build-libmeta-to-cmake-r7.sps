(import (scheme base)
        (scheme cxr)
        (scheme file)
        (scheme write)
        (scheme read)
        (scheme process-context))

(define (datum->string x)
  (let ((p (open-output-string)))
   (write x p)
   (get-output-string p)))

(define (file->list proc pth)
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
    (display "Usage: build-libmeta-to-cmake.sps <liblist.scm> <out.cmake>\n")
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
        ((number? x)
         (number->string x))
        (else (datum->string x))))
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
    (datum->string spec)))

(define (libname->cmakesym spec)
  (define (conv elem)
    (datum->string elem))
  (define (itr acc cur)
    (if (pair? cur)
      (itr (string-append acc "_" (conv (car cur)))
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

(define (strip-rename exports)
  (define (itr acc rest)
    (if (pair? rest)
      (let ((x (car rest))
            (next (cdr rest)))
        (if (pair? x)
          (let ((a (car x))
                (d (cdr x)))
            (unless (eq? 'rename a)
              (error "Unknown export format" exports))
            (itr (append acc (map cadr d)) next))
          (itr (cons x acc) next)))
      acc))
  (itr '() exports))

(define (strip-import import)
  (unless (pair? import)
    (error "Unknown import format" import))

  (let ((a (car import)))
   (case a
     ((only except rename for) (strip-import (cadr import)))
     ;; FIXME: Handle primive for now (we should remove them)
     ((primitive) #f)
     (else import))))

(define (strip-imports imports)
  (let loop ((acc '()) (imports imports))
   (if (pair? imports)
     (let ((e (strip-import (car imports))))
      (if e
        (loop (cons e acc) (cdr imports))
        (loop acc (cdr imports))))
     acc)))

(define (proc in p)
  (let ((code (file->sexp-list in)))
   (unless (and (pair? code) (list? code) (= 1 (length code)))
     (error "Invalid library format" in code))
   (let* ((lib (car code))
          (head (car lib))
          (name (cadr lib))
          (exports (cdr (caddr lib)))
          (imports (cdr (cadddr lib)))
          (importlibs (strip-imports imports)))

     (let ((libsym (libname->cmakesym name)))
      ;; Existence check
      (say p "set(libfilename_" libsym " \"" in "\")\n")
      ;; Deps
      (say p "set(libdeps_" libsym "\n")
      (for-each (lambda (e)
                  (say p "  "
                       (libname->cmakesym e) "\n"))
                importlibs)
      (say p ")\n")
      ;; Exports
      (say p "set(libexports_" libsym "\n")
      (for-each (lambda (e)
                  (say p "  " 
                       (quotstr (libspec->string e))
                       "\n"))
                exports)
      (say p ")\n")
      ;; Exportsyms (stripped)
      (say p "set(libexportsyms_" libsym "\n")
      (for-each (lambda (e)
                  (say p "  " 
                       (quotstr (libspec->string e))
                       "\n"))
                (strip-rename exports))
      (say p ")\n")
      ;; Imports
      (say p "set(libimports_" libsym "\n")
      (for-each (lambda (e)
                  (say p "  " 
                       (quotstr (libspec->string e))
                       "\n"))
                imports)
      (say p ")\n")))))


;; Main
(let ((in (calc-input-file))
      (out (calc-output-file)))
  (let ((lis (car (file->sexp-list in))))
   (when (file-exists? out)
     (delete-file out))
   (call-with-output-file
     out
     (lambda (p)
       (for-each
         (lambda (e)
           (proc e p))
         lis)))))

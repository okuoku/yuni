(library (yuniapp tmpl cmdline)
         (export
           cmdline-win32)
         (import (yuni scheme)
                 (yuniconfig build))

(define (params sep l)
  (define (itr acc l)
    (if (pair? l)
      (let ((a (car l)))
       (itr (string-append acc sep a) (cdr l))
       )
      acc)) 
  (if (pair? l)
    (itr (car l) (cdr l))
    "")) 

(define (gen-libs param l)
  (define (itr acc l)
    (if (pair? l)
      (let ((a (car l)))
       (itr (string-append acc " " param " " a) (cdr l)))
      acc))
  (if (pair? l)
    (itr (string-append param " " (car l)) (cdr l))
    ""))

(define (quote-exec pth)
  (string-append "\"" pth "\""))

(define (invoke-cmd impl libpaths progpath initargs)
  (define cmd (quote-exec (yuniconfig-executable-path impl)))
  (define runtime (yuniconfig-runtime-rootpath))
  (define platform (yuniconfig-platform))
  (define all-libpaths (cons (string-append runtime "/" 
                                            (symbol->string impl))
                             libpaths))

  (case impl
    ((racket)
     (string-append cmd " -I scheme/init -l- r6rs/run.rkt "
                    (gen-libs "++path" all-libpaths)
                    " "
                    progpath
                    (params " " initargs)))
    (else
      (error "Unknown implementation" impl))))

(define (cmdline-win32 impl libpaths progpath initargs)
  (define cmdraw (invoke-cmd impl libpaths progpath initargs))
  (string-append
    "@echo off\n\n"
    cmdraw
    " %*\n"))

)

(library (yuniapp tmpl cmdline)
         (export
           cmdline-sh
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

(define (%gen-libs0 param l)
  (define (itr acc l)
    (if (pair? l)
      (let ((a (car l)))
       (itr (string-append acc " " param a) (cdr l)))
      acc))
  (if (pair? l)
    (itr (string-append param (car l)) (cdr l))
    ""))

(define (gen-libs/nospace param l)
  (%gen-libs0 param l))

(define (gen-libs param l)
  (%gen-libs0 (string-append param " ") l))

(define (gen-pathsep pathsep l)
  (define (itr acc l)
    (if (pair? l)
      (let ((a (car l)))
       (itr (string-append acc pathsep a) (cdr l)))
      acc))
  (if (pair? l)
    (itr (car l) (cdr l))
    ""))

(define (quote-exec pth)
  (string-append "\"" pth "\""))

(define (mapgen sym)
  (case sym
    ((gauche) "gosh")
    ((chez) "chez-scheme")
    (else (symbol->string sym))))

(define (invoke-cmd win32? impl loaderroot libpaths progpath initargs)
  (define cmd (quote-exec (yuniconfig-executable-path impl)))
  (define runtime (yuniconfig-runtime-rootpath))
  (define platform (yuniconfig-platform))
  (define all-libpaths (cons (string-append runtime "/" 
                                            (symbol->string impl))
                             libpaths))
  (define args (params " " initargs))
  (define pathsep (if win32? ";" ":"))

  (case impl
    ((guile)
     (string-append cmd " "
                    (gen-libs "-L" all-libpaths)
                    " "
                    progpath
                    args))
    ((vicare)
     (string-append cmd " " (gen-libs "--source-path" all-libpaths)
                    " --r6rs-script " progpath
                    " -- " args))
    ((chez)
     (string-append cmd " --libdirs " (gen-pathsep pathsep all-libpaths)
                    " --program " progpath args))
    ((sagittarius)
     (string-append cmd " " (gen-libs/nospace "--loadpath=" all-libpaths)
                    " "
                    progpath
                    args))
    ((chibi-scheme)
     (string-append cmd " " (gen-libs "-I" all-libpaths)
                    " "
                    progpath
                    args))
    ((racket)
     (string-append cmd " -I scheme/init -l- r6rs/run.rkt "
                    (gen-libs "++path" all-libpaths)
                    " "
                    progpath
                    args))
    ((gauche)
     (string-append cmd " " (gen-libs "-I" all-libpaths)
                    " "
                    progpath
                    args))
    (else
      (error "Unknown implementation" impl))))

(define (cmdline-win32 impl loaderroot approot libpaths progpath initargs)
  (define cmdraw (invoke-cmd #t impl loaderroot libpaths progpath initargs))
  (define genscr (string-append "gen-" (mapgen impl) ".bat"))
  (string-append
    "@echo off\n\n"
    loaderroot "/../" genscr " " approot
    " && "
    cmdraw
    " %*\n"))

(define (cmdline-sh impl loaderroot approot libpaths progpath initargs)
  (define cmdraw (invoke-cmd #f impl loaderroot libpaths progpath initargs))
  (define genscr (string-append "gen-" (mapgen impl)))
  (string-append
    "#!/bin/sh\n\n"
    loaderroot "/../" genscr " " approot
    " && "
    "exec "
    cmdraw
    " $*\n"))

)

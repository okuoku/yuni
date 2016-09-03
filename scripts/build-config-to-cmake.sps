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
    (display "Usage: build-config-to-cmake.sps <config-file>\n")
    (exit #f))
  (let ((l (command-line)))
   (unless (< 1 (length l)) (usage))
   (let ((fn (cadr l)))
    (unless (file-exists? fn)
      (usage))
    fn)))

(define config (file->sexp-list (calc-input-file)))

(define *library-groups* #f)
(define GenR6RSCommon #f)
(define GenR7RS #f)
(define GenRacket #f)

;; Load config
(for-each (lambda (frm)
            (let ((a (car frm))
                  (d (cdr frm)))
              (case a
                ((*library-groups*)
                 (set! *library-groups* d))
                ((GenR6RSCommon)
                 (set! GenR6RSCommon d))
                ((GenR7RS)
                 (set! GenR7RS d))
                ((GenRacket)
                 (set! GenRacket d))
                (else 'ignore))))
          config)

;; Output utils
(define (say p . x)
  (for-each (lambda (x) 
              (if (list? x)
                (apply say p x)
                (display x p)))
            x))
(define (quotsym x)
  (string-append "\"" (symbol->string x) "\" "))

;; Emit libgroups.cmake
(when (file-exists? "libgroups.cmake")
  (delete-file "libgroups.cmake"))
(call-with-output-file
  "libgroups.cmake"
  (lambda (p)
    (say p "set(library_groups\n")
    (for-each
      (lambda (g)
        (say p "  " (quotsym g) "\n"))
      (map car *library-groups*))
    (say p ")\n")
    (for-each
      (lambda (grp)
        (let ((nam (car grp))
              (mappings (cdr grp)))
          (say p "set(library_group_" nam "\n")
          (for-each (lambda (mapping)
                      (cond
                        ((list? mapping)
                         (let ((from (symbol->string (car mapping)))
                               (arrow (cadr mapping))
                               (to (symbol->string (caddr mapping))))
                           (unless (eq? '=> arrow)
                             (error "Invalid mapping format" mapping))
                           (say p "  " (quotsym (string->symbol
                                                  (string-append
                                                    from
                                                    "'"
                                                    to)))
                                "\n")))
                        (else
                          (say p "  " (quotsym mapping) "\n"))))
                    mappings)
          (say p ")\n")))
      *library-groups*)))

;; Emit genmappings.cmake
(when (file-exists? "genmappings.cmake")
  (delete-file "genmappings.cmake"))
(call-with-output-file
  "genmappings.cmake"
  (lambda (p)
    (define (gen nam lis)
      (say p "set(" nam
           " "
           (map quotsym (map car lis))
           ")\n")
      (for-each (lambda (x)
                  (let ((head (car x))
                        (mapping (cdr x)))
                    (say p "set(" (string-append
                                  nam
                                  "-"
                                  (symbol->string head))
                         "\n")
                    (for-each (lambda (x)
                                (say p "  "
                                     (quotsym x)
                                     "\n"))
                              mapping)
                    (say p ")\n")))
                lis))
    (gen "GenRacket" GenRacket)
    (gen "GenR7RS" GenR7RS)
    (gen "GenR6RSCommon" GenR6RSCommon)))


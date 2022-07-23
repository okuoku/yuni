;; FIXME: Library scoping is not implemented yet.
;;        it's just a load-order generator

(import ;(scheme base)
        (scheme process-context)
        (scheme read)
        ;(scheme cxr)
        (scheme write)
        ;(scheme repl)
        (scheme file)
        (scheme eval)
        ;(scheme load)
        (scheme inexact)
        (scheme case-lambda)
        (scheme cyclone util)
        )


(define (%%extract-program-args args* entrypth)
  (if (string=? (car args*) entrypth)
    (cdr args*)
    (%%extract-program-args (cdr args*) entrypth)))

(define (%%extract-entrypoint-path args*)
  (define (checkone s)
    (and (string? s) 
         (let ((len (string-length s)))
          (and (< 4 len)
               (string=? (substring s (- len 4) len) ".scm")
               s))))
  (and (pair? args*)
       (or (checkone (car args*))
           (%%extract-entrypoint-path (cdr args*)))))

(define (%%pathslashfy pth)
  (let* ((l (string->list pth))
         (x (map (lambda (c) (if (char=? #\\ c) #\/ c)) l)))
    (list->string x)))

(define (%%pathsimplify pth)
  (define (pathcompose acc l)
    (if (pair? l)
      (pathcompose (if (string=? (car l) "")
                     acc
                     (string-append acc "/" (car l))) 
                   (cdr l))
      acc))
  (define (pathcompose-start acc l)
    (pathcompose (car l) (cdr l)))
  (define (pathcomponent acc cur strq)
    (if (string=? strq "")
      (if (null? acc)
        (reverse cur)
        (reverse (cons (list->string (reverse acc)) cur)))
      (let ((c (string-ref strq 0))
            (r (substring strq 1 (string-length strq))))
        (if (char=? c #\/)
          (pathcomponent '() (cons (list->string (reverse acc)) cur) r)
          (pathcomponent (cons c acc) cur r)))))
  (define (simple cur m q)
    ;(write (list 'simple cur m q)) (newline)
    (if (null? q)
      (if (null? cur)
        (reverse (cons m cur))
        (reverse (cdr cur)))
      (let ((a (car q))
            (d (cdr q)))
        (if (string=? ".." a)
          (let ((next-cur (if (null? cur)
                            (list "..")
                            (cdr cur))))
            (if (null? d)
              (reverse next-cur)
              (simple next-cur (car d) (cdr d))))
          (simple (cons m cur) a d)))))
  (define (start-simple cur m q)
    ;; Protect relative ../../../ sequence at beginning
    (if (string=? m "..")
      (start-simple (cons m cur) (car q) (cdr q))
      (simple cur m q)))

  (let ((r (pathcomponent '() '() pth)))
   (pathcompose-start "" (start-simple '() (car r) (cdr r)))))


(define (%%locate-yuniroot-fromscmpath scmpath)
  (define MYNAME "selfboot-entry.scm")
  (write %%selfboot-orig-command-line) (newline)
  (write %%selfboot-mypath) (newline)
  (let ((npth (%%pathslashfy scmpath)))
   (%%pathsimplify (string-append npth "/../../../.."))))

(define (%%drop-last lis) (reverse (cdr (reverse lis))))

(define %%selfboot-orig-command-line (%%drop-last (command-line)))
(define %%selfboot-mypath (%%extract-entrypoint-path %%selfboot-orig-command-line))
(define %%selfboot-yuniroot (%%locate-yuniroot-fromscmpath %%selfboot-mypath))
(define %%selfboot-program-args (%%extract-program-args
                                  %%selfboot-orig-command-line
                                  %%selfboot-mypath))

;; Porting (Cyclone)
(define (yuni/gensym sym) (gensym sym))
(define myenv (interaction-environment))
(define %selfboot-file-exists? file-exists?)
(define %%selfboot-impl-type 'cyclone)
(define %%selfboot-core-libs '((scheme base)
                               (scheme case-lambda)
                               (scheme cxr)
                               (scheme file)
                               (scheme inexact)
                               (scheme process-context)
                               (scheme read)
                               (scheme write)
                               (scheme eval)
                               (srfi 69)
                               (yuni scheme)
                               ))

(define (%selfboot-file->sexp-list fn)
  (call-with-input-file
    fn
    (lambda (p)
      (let loop ((cur '()))
       (let ((r (read p)))
        (if (eof-object? r)
          (reverse cur)
          (loop (cons r cur))))))))

(define (%%selfboot-load-runtime path)
  (let ((p (string-append %%selfboot-yuniroot path)))
   (load p myenv)))

;; Setup environment
(when (string=? %%selfboot-yuniroot "")
  (set! %%selfboot-yuniroot "."))

;:: common selfboot libraries
;; selfboot/common/pathname.scm

(define (%selfboot-match-ext? ext fn)
  (let ((len (string-length fn)))
   (and (< 3 len)
        (string=? ext (stubstring fn (- len 4) len)))))

(define (%selfboot-is-sls? fn) (%selfboot-match-ext? ".sls" fn))

;; selfboot/common/library-walk.scm

(define (%selfboot-gen-loadorder libread libcheck initial-dep)
  ;; => (path libname ...)
  ;; (libread LIBNAME) => sexp
  ;; (libcheck LIBNAME) => LIBNAME(can be aliased) / #f(ignore)
  (define order '())
  (define (libnamestrip libname)
    (if (pair? libname)
      (let ((sy (car libname)))
       (case sy
         ((rename except only for)
          (libnamestrip (cadr libname)))
         (else libname)))
      libname))
  (define (libname=? a b)
    (cond
      ((and (null? a) (null? b)) #t)
      (else
        (and (pair? a)
             (pair? b)
             (let ((aa (car a))
                   (bb (car b)))
               (and (eqv? aa bb)
                    (libname=? (cdr a) (cdr b))))))))
  (define (libname=?/list lis nam)
    (and (pair? lis)
         (let ((a (car lis)))
          (or (libname=? a nam)
              (libname=?/list (cdr lis) nam)))))
  (define (is-loaded? lib)
    (let loop ((q order))
     (and (pair? q)
          (let ((n (caar q))
                (next (cdr q)))
            (or (libname=?/list n lib)
                (loop next))))))
  (define (tryload! lib)
    (let* ((usagename (libnamestrip lib))
           (truename (libcheck usagename)))
     (when (and truename (not (is-loaded? truename)))
       (let* ((code (libread truename))
              (deps (%selfboot-library-depends code))
              (syms (%selfboot-library-exports code))
              (names (if (not (libname=? truename usagename))
                       (list truename usagename)
                       (list usagename))))
         (for-each tryload! deps)
         (set! order (cons (cons names (cons deps syms)) order))))))

  (for-each tryload! (map libnamestrip initial-dep))
  (set! order (reverse order))
  ;;(for-each (lambda (b) (write b) (newline)) order)
  order)

;; selfboot/common/library-parser.scm

(define (%selfboot-library-name sexp)
  (cadr sexp))

(define (%selfboot-library-depends sexp)
  (cdr (cadddr sexp)))

(define (%selfboot-library-exports sexp)
  (cdr (caddr sexp)))

(define (%selfboot-program-depends sexp)
  (cdr (car sexp)))

;; selfboot/common/polyfills.scm

(define (%%selfboot-yuniconfig-get-polyfill-path sym)
  (let ((n (assoc sym yuni/polyfills)))
   (cond
     (n (cdr n))
     (else
       (write (list 'UNKNOWN: sym)) (newline)
       (error "Unknown polyfill" sym)))))

(define yuni/polyfills
  '(
    ;; Std, non-R5RS
    (boolean=? . "lib-runtime/generic/std/boolean_eqp.scm")
    (floor-quotient . "lib-runtime/generic/std/floor-quotient.scm")
    (floor-remainder . "lib-runtime/generic/std/floor-remainder.scm")
    (floor/ . "lib-runtime/generic/std/floor_div.scm")
    (list-copy . "lib-runtime/generic/std/list-copy.scm")
    (list-set! . "lib-runtime/generic/std/list-set_x.scm")
    (make-list . "lib-runtime/generic/std/make-list.scm")
    (modulo . "lib-runtime/generic/std/modulo.scm")
    (quotient . "lib-runtime/generic/std/quotient.scm")
    (remainder . "lib-runtime/generic/std/remainder.scm")
    (string-copy . "lib-runtime/generic/std/string-copy.scm")
    (string-for-each . "lib-runtime/generic/std/string-for-each.scm")
    (string-map . "lib-runtime/generic/std/string-map.scm")
    (string->list . "lib-runtime/generic/std/string_to_list.scm")
    (string->vector . "lib-runtime/generic/std/string_to_vector.scm")
    (truncate-quotient . "lib-runtime/generic/std/truncate-quotient.scm")
    (truncate-remainder . "lib-runtime/generic/std/truncate-remainder.scm")
    (truncate/ . "lib-runtime/generic/std/truncate_div.scm")
    (vector-append . "lib-runtime/generic/std/vector-append.scm")
    (vector-copy . "lib-runtime/generic/std/vector-copy.scm")
    (vector-copy! . "lib-runtime/generic/std/vector-copy_x.scm")
    (vector-fill! . "lib-runtime/generic/std/vector-fill_x.scm")
    (vector-for-each . "lib-runtime/generic/std/vector-for-each.scm")
    (vector-map . "lib-runtime/generic/std/vector-map.scm")
    (vector->list . "lib-runtime/generic/std/vector_to_list.scm")
    (vector->string . "lib-runtime/generic/std/vector_to_string.scm")
    ))

;; selfboot/common/yuniconfig.scm

(define (%selfboot-yuniconfig-resolver-add-loadpath! p path)
  (p (list path)))
(define (%selfboot-yuniconfig-gen-resolver impl yuniroot)
  (define cfgpath (string-append yuniroot "/config/config.scm"))
  (let* ((cfg (%selfboot-file->sexp-list cfgpath))
         (libdirs
           (map (lambda (p) (string-append yuniroot "/" p))
                (cdr (assoc '*library-directories* cfg))))
         (libgrps (cdr (assoc '*library-groups* cfg)))
         (g1 (cdr (assoc 'GenRacket cfg)))
         (g2 (cdr (assoc 'GenR7RS cfg)))
         (g3 (cdr (assoc 'GenR6RSCommon cfg)))
         (implgroups (append g1 g2 g3)))
    (let ((mygroups (cdr (assoc impl implgroups))))
     (define prefixes '())
     (define (add-prefix! e)
       (cond
         ((pair? e)
          (unless (and (= (length e) 3) (eq? (cadr e) '=>))
            (error "Unknown libent" e))
          (let ((aliasname (car e))
                (origname (caddr e)))
            (let ((p (assoc origname prefixes)))
             (cond
               (p
                 (let ((d (cdr p)))
                  (set-cdr! p (cons aliasname d))))
               (else
                 (set! prefixes (cons (list origname aliasname) prefixes)))))))
         (else
           (unless (symbol? e)
             (error "Unknown libent" e))
           (let ((p (assoc e prefixes)))
            (unless p
              (set! prefixes (cons (list e e) prefixes)))))))

     (for-each (lambda (group)
                 (let ((prefix (assoc group libgrps)))
                  (unless prefix
                    (error "Prefix not found!" group))
                  (let ((lis (cdr prefix)))
                   (for-each add-prefix! lis))))
               mygroups)

     (lambda (arg)
       (define (libname->path libname)
         (let ((n (reverse libname)))
          (let loop ((q (cdr n))
                     (cur (symbol->string (car n))))
            (if (null? q)
              (string-append cur ".sls")
              (loop (cdr q)
                    (string-append (symbol->string (car q)) "/" cur))))))
       (define (try-lib libname) ;; #f / (ORIGNAME ALIASNAME DIR PTH)
         (let ((pth (libname->path libname)))
          (let loop ((q libdirs))
           (and (not (null? q))
                (let ((prefix (car q))
                      (next (cdr q)))
                  ;(write (list 'TRYLIB: libname prefix pth)) (newline)
                  (if (%selfboot-file-exists?
                        (string-append prefix "/" pth))
                    (list arg libname prefix pth)
                    (loop next)))))))
       (let ((a (car arg))
             (d (cdr arg)))
        (cond
          ((string? a)
           (set! libdirs (cons a libdirs)))
          ((symbol? a)
           (let ((aliases (or (assoc a prefixes) (list a))))
            (let loop ((q aliases))
             (cond
               ((null? q) #f)
               (else (let ((a (car q))
                           (next (cdr q)))
                       (let ((e (try-lib (cons a d))))
                        (or e
                            (loop next)))))))))))))))

(define (%selfboot-yuniconfig-get-runtime-list impl yuniroot)
  ;; => runtime file paths relative to yuniroot
  (define cfgpath (string-append yuniroot "/config/generic-runtime.scm"))
  (let ((cfg (%selfboot-file->sexp-list cfgpath)))
   (let ((generic (assoc 'generic cfg))
         (additional (assoc impl cfg)))
     (if additional
       (append
         (cadr additional)
         (cadr generic)
         (caddr generic)
         (caddr additional))
       '()))))

;; selfboot/common/genfilelist.scm
(define (%%selfboot-gen-filelist loadpath* entrypoint*)
  (define deps* '())
  (define (ignore-lib? lib)
    (define (itr rest)
      (and (pair? rest)
           (or (equal? (car rest) lib)
               (itr (cdr rest)))))
    (itr %%selfboot-core-libs))
  (define resolver
    (%selfboot-yuniconfig-gen-resolver
      %%selfboot-impl-type
      %%selfboot-yuniroot))

  (define (libread libname)
    ;(write (list 'libread: libname)) (newline)
    (let ((r (resolver libname)))
     (unless r
       (error "cannot read" libname))
     (let ((orig (car r))
           (aliasname (cadr r))
           (dir (caddr r))
           (pth (cadddr r)))
       (car (%selfboot-file->sexp-list (string-append dir "/" pth))))))

  (define (libcheck0 libname)
    (let ((r (resolver libname)))
     (unless r
       (error "Cannot read" libname))
     (let ((orig (car r))
           (aliasname (cadr r))
           (dir (caddr r))
           (pth (cadddr r)))
       aliasname)))

  (define (libcheck libname)
    ;(write (list 'libcheck: libname)) (newline)
    (cond
      ((ignore-lib? libname) #f)
      (else (libcheck0 libname))))

  ;; Add loadpath
  (for-each
    (lambda (path)
      (%selfboot-yuniconfig-resolver-add-loadpath!
        resolver path))
    loadpath*)

  ;; Collect deps
  (for-each
    (lambda (path)
      (let ((code (%selfboot-file->sexp-list path)))
       (set! deps* (append (%selfboot-program-depends code) deps*))))
    entrypoint*)

  ;; Generate liborder
  (let ((order (%selfboot-gen-loadorder libread libcheck deps*))
        (runtimefiles (%selfboot-yuniconfig-get-runtime-list
                        %%selfboot-impl-type
                        %%selfboot-yuniroot)))
    (append
      (map (lambda (path)
             (cond
               ((symbol? path)
                ;; Polyfill
                (let ((actual-path (%%selfboot-yuniconfig-get-polyfill-path
                                     path)))
                  (list #f %%selfboot-yuniroot actual-path #f path)))
               (else
                 (list #f %%selfboot-yuniroot path #f (cons #f #f)))))
           runtimefiles)
      (map (lambda (libinfo)
             (let ((names (car libinfo))
                   (exports (cdr libinfo)))
              (let* ((libname (car names))
                     (r (resolver libname)))
                (let ((dir (caddr r))
                      (pth (cadddr r)))
                  (if (pair? (cdr names))
                    (list libname dir pth (cdr names) exports)
                    (list libname dir pth #f exports))))))
           order))))


;;; Loader/Runner

;; (part from) selfboot/common/run-program.scm
(define %%selfboot-current-command-line %%selfboot-program-args)
(define %%selfboot-current-libpath (list %%selfboot-yuniroot))
(define (yuni/command-line) %%selfboot-current-command-line)

;; Scan arguments
(let loop ((q %%selfboot-current-command-line))
 (if (pair? q)
   (let ((a (car q))
         (d (cdr q)))
     (cond
       ((string=? "-LIBPATH" a)
        (let ((dir (car d)))
         (set! %%selfboot-current-libpath
           (cons dir
                 %%selfboot-current-libpath))
         (loop (cdr d))))
       (else
         (set! %%selfboot-current-command-line q))))
   #t))

;; Collect deps and load
(let ((r (yuni/command-line))
      (code* '()))
 (let* ((prog (car r))
        (codetab (%%selfboot-gen-filelist
                   %%selfboot-current-libpath
                   (list prog))))
   (for-each (lambda (e)
               (let ((dir (cadr e))
                     (pth (caddr e))
                     (truename (car e))
                     (aliasnames (cadddr e))
                     (libexports (car (cddddr e))))
                 (cond
                   ((pair? libexports)
                    (let ((raw-imports (car libexports))
                          (exports (cdr libexports))
                          (filepth (string-append dir "/" pth)))
                      (write (list 'LOAD: filepth)) (newline)
                      (let* ((n (%selfboot-file->sexp-list filepth))
                             (libcode* (cddr (cddr (car n)))))
                       ;; Push program
                       (set! code*
                         (cons (cons 'begin libcode*) code*)))))
                   (else ;; Polyfill
                     (error "We cannot load polyfill here")))))
             codetab)
   ;;
   (write (list 'LOAD: prog)) (newline)
   (set! code* (cons (cons 'begin 
                           ;; Strip (import ...)
                           (cdr (%selfboot-file->sexp-list prog))) code*))
   ;; override command-line
   (let ((command-line-code 
           `(begin (define (command-line) (quote ,%%selfboot-program-args)))))
     (eval (cons command-line-code
                 (cons 'begin (reverse code*)))))))

;; Exit successfully if the program does not have exit call
(exit 0)


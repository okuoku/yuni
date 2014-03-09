(library (yuni text tabular)
         (export format-tabular
                 tabular->string)
         (import (rnrs)
                 (yuni core)
                 (srfi :8)
                 (srfi :26)
                 (srfi :42)
                 (srfi :48)
                 (shorten)
                 (match))

;; Tabular:
;;  #(spec ...)
;;  spec = (format-spec . width-spec) | format-spec | #f
;;  width-spec = (FIXED-WIDTH) | (LEAST-WIDTH +)
;;
;;  Format-spec: 
;;    "title" '~' "format"
;;
;;  Sep: (FIXME: not Implemented yet)
;;    / = Consume next datum
;;    ! = Prefix String
;;    _ = Suffix String
;;
;;  Data class:
;;    s = String
;;    d = decimal number
;;    x = hexadecimal number
;;    b = binary number
;;    (f = floating point number :: FIXME: Implement it...)
;;    (g = floating point number, gauge :: FIXME: Implement it...)
;;
;;  Number formatter:
;;
;;  Alignment:
;;    C = Center
;;    c = Center
;;    L = Left
;;    R = Right
;;
;;  Rule:
;;    Wx = Add x as West rule charactor
;;    Ex = Add x as East rule charactor

(define* rowspec (title 
                   type 
                   align-title 
                   align-datum 
                   fixed-width?
                   least-width
                   number-width-int 
                   number-width-prec
                   rule-west
                   rule-east
                   ))

(define* (format-straight (rowspec) datum)
  (let-with rowspec (type)
    (case type
      ((string)
       (format "~a" datum))
      ((hexadecimal)
       (format "~x" datum))
      ((binary)
       (format "~b" datum))
      ((decimal)
       (format "~a" datum))
      (else
        (assertion-violation 'format-straight
                             "Unknown format"
                             type)))))

(define* (ok-to-chop? (rowspec))
  (let-with rowspec (type)
    (case type
      ((string) #t)
      (else #f))))

;; FIXME: Do it more elegant way...
(define (fit-to0 target w*) 
  (let* ((count (length w*))
         (total (fold-left + 0 w*))
         (dif (- target total))
         (adif (abs dif))
         (zdif (floor (/ adif count)))
         (hcnt (- adif (* zdif count)))
         (m (if (< dif 0) (- zdif) zdif))
         (m+ (if (< dif 0) (- m 1) (+ m 1)))) 
    (list-ec (: i count)
             (let ((w (list-ref w* i)))
               (if (< i hcnt)
                 (+ m+ w)
                 (+ m w)))))) 

(define (fit-to target w* resize-map)
  (define wx '())
  (define dif 0)
  (define (ret cur resized const m)
    (if (pair? m)
      (let ((swap? (car m)))
        (if swap?
          (ret (cons (car resized) cur) (cdr resized) (cdr const) (cdr m))
          (ret (cons (car const) cur) resized (cdr const) (cdr m))))
      (reverse cur)))

  (for-each (^[w r] 
              (when r (set! wx (cons w wx)))
              (unless r (set! dif (+ dif w)))) w* resize-map)
  (let ((w (fit-to0 (- target dif) (reverse wx))))
    (ret '() w w* resize-map)))

(define (apply-align str sym w)
  (define (space x) (make-string x #\space))
  (define dif (- w (string-length str)))
  (cond
    ((= dif 0) str)
    ((> dif 0)
     (case sym
       ((center)
        (let* ((m (/ dif 2))
               (ldif (ceiling m))
               (rdif (floor m)) )
          (string-append (space ldif) str (space rdif))))
       ((left)
        (string-append str (space dif)))
       ((right)
        (string-append (space dif) str))))
    (else
      (assertion-violation 'apply-align
                           "doesn't fit"
                           (list str w)
                           sym))))

(define (pass0 tbl)
  ;; Remove ignored column
  (define spec (vector->list (car tbl)))
  (define obj (cdr tbl))
  (define (spec-filter e)
    (reverse (fold-left (^[cur e s] (if s (cons e cur) cur))
                        '()
                        e
                        spec)))
  (cons (list->vector (filter (^e e) spec))
        (map spec-filter obj)))

(define* (format-tabular #((width-limit: #f)
                           (fixed-width: #f)) tbl)
  (define l (pass0 tbl))
  (define (list-transpose size lis)
    ;(display (list 'transpose: lis))(newline)
    (list-ec (: i size)
             (map (cut list-ref <> i) lis)))
  (define (calc-pass1-width lis)
    ;(display (list 'calc-pass1: lis))(newline)
    (map (cut apply max <>) lis))

  (define* (check-least-width (rowspec) w)
    (let-with rowspec (least-width fixed-width?)
      (cond
        (fixed-width? least-width)
        ((and least-width (< w least-width))
         least-width)
        (else w))))

  (receive (headers data0) (partition vector? l)
    (unless (= 1 (length headers))
      (assertion-violation 'format-tabular
                           "Too many headers"
                           headers))
    ;; Pass0: Instanciate rowspecs
    (let* ((rowspecs0 (map make-rowspec (vector->list (car headers)))) 
           ;; Expand rules(Insert rule strings)
           (rowspecs (expand-rowspecs rowspecs0))
           (data (expand-data rowspecs0 data0))
           ;; Pass1:
           (pass1/titles (map (^e (~ e 'title)) rowspecs))
           (data-t (list-transpose (length rowspecs) data))
           ;; Format with free width
           (pass1 (map (^[r l] (map (^e (format-straight r e)) l)) 
                       rowspecs data-t))
           (pass1/lengths (map (^[r e] (map (cut string-length <>) (cons r e))) 
                               pass1/titles pass1))
           (pass1/widths0 (calc-pass1-width pass1/lengths))
           ;; Apply least-width
           (pass1/widths (map check-least-width rowspecs pass1/widths0))
           (current-width (fold-left + 0 pass1/widths)))
      (define widths pass1/widths)
      ;(display (list 'len: pass1/lengths))(newline)
      ;(display (list 'width0: pass1/widths))(newline)
      ;(display (list 'width: pass1/widths))(newline)
      (cond
        (width-limit:
          (when (< width-limit: current-width)
            ;; We have to shorten vari-length fields
            (set! widths (fit-to width-limit: pass1/widths 
                                 (map ok-to-chop? rowspecs)))))
        (fixed-width:
          (when (> fixed-width: current-width)
            ;; Resize to fixed-width
            (set! widths (fit-to fixed-width: pass1/widths 
                                 (map (^_ #t) rowspecs))))))
      ;; Pass2: Expand and align to calculated width
      (let* ((titles (map (^[title r width] 
                            (apply-align title (~ r 'align-title) width))
                          pass1/titles rowspecs widths)) 
             (pass2 (map (^[title r width l] 
                           (cons title
                                 (let ((align (~ r 'align-datum)))
                                   (map (^e (apply-align e align width)) l)))) 
                         titles rowspecs widths pass1)))
        (list-transpose 
          (length l)
          pass2))))) 

(define (expand-rowspecs rowspecs)
  (define (new str)
    (define me (new-rowspec))
    (~ me 'type := 'string)
    (~ me 'fixed-width? := #t)
    (~ me 'least-width := (string-length str))
    me)
  (define (expand e) ;; => list
    (define (left)
      (let-with e (rule-west)
        (if (string=? "" rule-west)
          '()
          (list (new rule-west)))))
    (define (right)
      (let-with e (rule-east)
        (if (string=? "" rule-east)
          '()
          (list (new rule-east)))))

    (append (left)
            (list e)
            (right)))
  (apply append (map expand rowspecs)))

(define (expand-data rowspecs0 data)
  (define (expand e) ;; => list
    (define (left)
      (let-with e (rule-west)
        (if (string=? "" rule-west)
          '()
          (list rule-west))))
    (define (right)
      (let-with e (rule-east)
        (if (string=? "" rule-east)
          '()
          (list rule-east))))
    (append (left)
            (list #f) ;; Consume
            (right)))
  (define code (apply append (map expand rowspecs0)))
  (define (expand1 e)
    (define (itr cur rest code*)
      (if (pair? code*)
        (let ((c (car code*))
              (d (cdr code*)))
          (if c
            (itr (cons c cur) rest d)
            (itr (cons (car rest) cur) (cdr rest) d)))
        (reverse cur)))
    (itr '() e code))
  (map expand1 data))

(define (new-rowspec)
  (make rowspec
        (title "")
        (type 'string)
        (align-title 'center)
        (align-datum #f)
        (least-width 0)
        (fixed-width? #f)
        (number-width-int 0)
        (number-width-prec 0)
        (rule-east "")
        (rule-west "")))

(define (make-rowspec dat)
  (define str (if (pair? dat) (car dat) dat))
  (define width-spec (if (pair? dat) (cdr dat) '()))
  (define (extract-title str)
    (define (itr cur rest)
      (match rest
             ((#\\ #\~ . next)
              (itr cur next))
             ((c #\~ . next)
              (values (list->string (reverse (cons c cur)))
                      (list->string next)))
             ((c . next)
              (itr (cons c cur) next))
             (else ;; No title
               (values "" str))))
    (itr '() (string->list str)))

  (define (apply-width width-spec)
    (match width-spec
           ((width)
            (~ me 'fixed-width? := #t)
            (~ me 'least-width := width))
           ((width '+)
            (~ me 'fixed-width? := #f)
            (~ me 'least-width := width))
           (else ;; No width spec
             'ok)))

  (define reg 0)
  (define (apply-align sym)
    (if (~ me 'align-datum)
      (~ me 'align-title := sym)
      (~ me 'align-datum := sym)))

  (define consume-char #f)
  (define (procchar c)
    (cond
      (consume-char
        (case consume-char
          ((rule-west)
           (let-with me (rule-west)
             (~ me 'rule-west := (string-append 
                                   rule-west
                                   (list->string (list c))))))
          ((rule-east)
           (let-with me (rule-east)
             (~ me 'rule-east := (string-append 
                                   rule-east
                                   (list->string (list c)))))))
        (set! consume-char #f))
      (else
        (case c
          ((#\W)
           (set! consume-char 'rule-west))
          ((#\E)
           (set! consume-char 'rule-east))
          ((#\C #\c)
           (apply-align 'center))
          ((#\L)
           (apply-align 'left))
          ((#\R)
           (apply-align 'right))
          ((#\s)
           (~ me 'type := 'string))
          ((#\d)
           (~ me 'type := 'decimal))
          ((#\x)
           (~ me 'type := 'hexadecimal))
          ((#\b)
           (~ me 'type := 'binary))))))


  (define me (new-rowspec))
  ;; First, extract title
  (receive (title fmt) (extract-title str)
    (~ me 'title := title)
    ;; Apply format spec
    (for-each procchar (string->list fmt)))
  (unless (~ me 'align-datum)
    (~ me 'align-datum := 'right))
  ;; Apply width spec
  (apply-width width-spec)
  me)

(define (tabular->string . x)
  (define out (apply format-tabular x))
  (receive (port proc) (open-string-output-port)
    (define title-line (car out))
    (define rows (length title-line))
    (define contents (cdr out))
    (define (put s) (display s port))
    ;; Print title line
    (put (car title-line))
    (for-each 
      (^e
        (put " ")
        (put e))
      (cdr title-line))
    (put "\n")
    ;; rule
    (put (make-string (+ rows (fold-left + 0 (map string-length title-line)))
                      #\=))
    (put "\n")

    ;; Print contents
    (for-each
      (^c
        (put (car c))
        (for-each
          (^e
            (put " ")
            (put e))
          (cdr c))
        (put "\n"))
      contents)
    (proc)))

)

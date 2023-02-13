(library (yuni io drypack)
         (export
           drypack-put
           drypack-get)
         (import (yuni scheme)
                 (yuni hashtables)
                 (yuni io leb128))

;;

(define (make-counter use-equiv?)
  (vector
    ;; Content size
    0
    ;; In ht
    (if use-equiv?  
      (make-eqv-hashtable)
      (make-eq-hashtable))
    ;; Out ht
    (make-eqv-hashtable)
    ;; Offset
    #f))

(define (counter-enter! ctr obj) ;; => already-seen?
  ;; NB: Don't insert Zone0 objects
  ;;     (Can't distinguish with #f)
  (let ((cnt (vector-ref ctr 0))
        (ht1 (vector-ref ctr 1))
        (ht2 (vector-ref ctr 2)))
    (let ((r (hashtable-ref ht1 obj #f)))
     (cond
       (r #t)
       (else
         (hashtable-set! ht1 obj cnt)
         (hashtable-set! ht2 cnt obj)
         (vector-set! ctr 0 (+ cnt 1))
         #f)))))

(define (counter-get ctr num)
  (hashtable-ref (vector-ref ctr 2) num #f))
(define (counter-ident ctr obj)
  (+ (vector-ref ctr 3)
     (hashtable-ref (vector-ref ctr 1) obj #f)))
(define (counter-total ctr)
  (vector-ref ctr 0))
(define (counter-for-each ctr proc)
  (define total (counter-total ctr))
  (define (itr cur)
    (unless (= total cur)
      (proc (counter-get ctr cur))
      (itr (+ cur 1))))
  (itr 0))
(define (counter-set-offset! ctr off)
  (vector-set! ctr 3 off))

(define nan-ident (list 'nan-ident))

(define (drypack-put port obj)
  (define chars (make-counter #f))
  (define numbers (make-counter #t))
  (define symbols (make-counter #f))
  (define strings (make-counter #f))
  (define bytevectors (make-counter #f))
  (define pairs (make-counter #f))
  (define vectors (make-counter #f))
  (define (encode-zone0 obj)
    (cond
      ((null? obj) 0)
      ((eq? #t obj) 1)
      ((eq? #f obj) 2)
      ((eof-object? obj) 3)
      (else #f)))

  (define (encode obj)
    (cond
      ((pair? obj)
       (counter-ident pairs obj))
      ((vector? obj)
       (counter-ident vectors obj))
      ((char? obj)
       (counter-ident chars obj))
      ((number? obj)
       (if (nan? obj)
           (counter-ident numbers nan-ident) 
           (counter-ident numbers obj) ))
      ((symbol? obj)
       (counter-ident symbols obj))
      ((string? obj)
       (counter-ident strings obj))
      ((bytevector? obj)
       (counter-ident bytevectors obj))
      (else
        (let ((r (encode-zone0 obj)))
         (unless r
           (error "Unrecognized object" obj))
         r))))

  (define (pass1 cur)
    (cond
      ((pair? cur)
       (unless (counter-enter! pairs cur)
         (pass1 (car cur))
         (pass1 (cdr cur))))
      ((vector? cur)
       (unless (counter-enter! vectors cur)
         (vector-for-each pass1 cur)))
      ((char? cur)
       (counter-enter! chars cur))
      ((number? cur)
       ;; Since +nan.0 cannot be used for a key, use specific object as a
       ;; proxy
       (if (nan? cur)
           (counter-enter! numbers nan-ident) 
           (counter-enter! numbers cur)))
      ((symbol? cur)
       (counter-enter! symbols cur))
      ((string? cur)
       (counter-enter! strings cur))
      ((bytevector? cur)
       (counter-enter! bytevectors cur))
      (else
        (unless (encode-zone0 cur)
          (error "Unrecognized object" cur)))))

  ;; Pass1: Scan input
  (pass1 obj)

  ;; Calc offsets
  (let ((offset 4)) ;; 4 = Zone0
   (counter-set-offset! chars offset)
   (set! offset (+ offset (counter-total chars)))
   (counter-set-offset! numbers offset)
   (set! offset (+ offset (counter-total numbers)))
   (counter-set-offset! symbols offset)
   (set! offset (+ offset (counter-total symbols)))
   (counter-set-offset! strings offset)
   (set! offset (+ offset (counter-total strings)))
   (counter-set-offset! bytevectors offset)
   (set! offset (+ offset (counter-total bytevectors)))
   (counter-set-offset! pairs offset)
   (set! offset (+ offset (counter-total pairs)))
   (counter-set-offset! vectors offset)
   (set! offset (+ offset (counter-total vectors)))

   ;; Output header
   (leb128-put port offset) ;; Total
   (leb128-put port (encode obj)) ;; Output
   (leb128-put port 4)
   (leb128-put port (counter-total chars))
   (leb128-put port (counter-total numbers))
   (leb128-put port (counter-total symbols))
   (leb128-put port (counter-total strings))
   (leb128-put port (counter-total bytevectors))
   (leb128-put port (counter-total pairs))
   (leb128-put port (counter-total vectors)))

  ;; Output static contents
  (counter-for-each 
    chars
    (lambda (c) (leb128-put port (char->integer c))))
  (counter-for-each
    numbers
    (lambda (n) (cond
                  ((and (exact? n) (integer? n) (positive? n))
                   (leb128-put port 1)
                   (leb128-put port n))
                  ((and (exact? n) (integer? n) (negative? n))
                   (leb128-put port 2)
                   (leb128-put port (- n)))
                  (else
                    (let* ((s (if (eq? n nan-ident)
                                  "+nan.0"
                                  (number->string n)))
                           (bv (string->utf8 s))
                           (len (bytevector-length bv)))
                      (leb128-put port 3)
                      (leb128-put port len)
                      (write-bytevector bv port))))))
  (counter-for-each
    symbols
    (lambda (s) 
      (let* ((bv (string->utf8 (symbol->string s)))
             (len (bytevector-length bv)))
        (leb128-put port len)
        (write-bytevector bv port))))
  (counter-for-each
    strings
    (lambda (s)
      (let* ((bv (string->utf8 s))
             (len (bytevector-length bv)))
        (leb128-put port len)
        (write-bytevector bv port))))
  (counter-for-each
    bytevectors
    (lambda (bv)
      (leb128-put port (bytevector-length bv))
      (write-bytevector bv port)))
  ;; Output pairs
  (counter-for-each
    pairs
    (lambda (p)
      (leb128-put port (encode (car p)))
      (leb128-put port (encode (cdr p)))))

  ;; Output vectors
  (counter-for-each
    vectors
    (lambda (v)
      (leb128-put port (vector-length v))
      (vector-for-each (lambda (e) 
                         (leb128-put port (encode e)))
                       v))))

(define (runfill offset cnt proc)
  (unless (= cnt 0)
    (proc offset)
    (runfill (+ offset 1) (- cnt 1) proc)))

(define (drypack-get port)
  (define total #f)
  (define output #f)
  (define temp #f)
  (define zone0-count #f)
  (define chars #f)
  (define numbers #f)
  (define symbols #f)
  (define strings #f)
  (define bytevectors #f)
  (define pairs #f)
  (define vectors #f)

  (define (get-bytes)
    (let ((len (leb128-get port)))
     (cond
       ((= len 0)
        (bytevector))
       (else
         (read-bytevector len port)))))

  ;; Read header
  (set! total (leb128-get port))
  (set! output (leb128-get port))
  (set! zone0-count (leb128-get port))
  (set! chars (leb128-get port))
  (set! numbers (leb128-get port))
  (set! symbols (leb128-get port))
  (set! strings (leb128-get port))
  (set! bytevectors (leb128-get port))
  (set! pairs (leb128-get port))
  (set! vectors (leb128-get port))

  ;; create buffer
  (set! temp (make-vector total))

  (unless (= 4 zone0-count)
    (error "Invalid header"))

  (let* ((pairoff (+ 4 chars numbers symbols strings
                     bytevectors))
         (vectoroff (+ pairoff pairs))
         (cur 0))
    ;; Zone0
    (vector-set! temp 0 '())
    (vector-set! temp 1 #t)
    (vector-set! temp 2 #f)
    (vector-set! temp 3 (eof-object))
    (set! cur 4)
    (runfill cur chars
             (lambda (idx)
               (vector-set! temp idx
                            (integer->char (leb128-get port)))))
    (set! cur (+ cur chars))
    (runfill cur numbers
             (lambda (idx)
               (let ((proto (leb128-get port)))
                (case proto
                  ((1) (vector-set! temp idx (leb128-get port)))
                  ((2) (vector-set! temp idx (- (leb128-get port))))
                  ((3) ;; Str
                   (let* ((bv (get-bytes))
                          (s (utf8->string bv))
                          (n (string->number s)))
                     (vector-set! temp idx n)))))))
    (set! cur (+ cur numbers))
    (runfill cur symbols
             (lambda (idx)
               (vector-set! temp idx (string->symbol 
                                       (utf8->string (get-bytes))))))
    (set! cur (+ cur symbols))
    (runfill cur strings
             (lambda (idx)
               (vector-set! temp idx (utf8->string (get-bytes)))))
    (set! cur (+ cur strings))
    (runfill cur bytevectors
             (lambda (idx)
               (vector-set! temp idx (get-bytes))))

    ;; Pass1 create idx pair/vectors
    (runfill pairoff pairs
             (lambda (idx)
               (let ((a (leb128-get port)))
                (let ((b (leb128-get port)))
                  (vector-set! temp idx (cons a b))))))

    (runfill vectoroff vectors
             (lambda (idx)
               (let* ((len (leb128-get port))
                      (v (make-vector len)))
                 (vector-set! temp idx v)
                 (runfill 0 len
                          (lambda (i)
                            (vector-set! v i (leb128-get port)))))))

    ;; Pass2 replace address into actual objects
    (runfill pairoff pairs
             (lambda (idx)
               (let* ((p (vector-ref temp idx))
                      (a (car p))
                      (d (cdr p)))
                (set-car! p (vector-ref temp a))
                (set-cdr! p (vector-ref temp d)))))
    (runfill vectoroff vectors
             (lambda (idx)
               (let* ((v (vector-ref temp idx))
                      (len (vector-length v)))
                 (runfill 0 len
                          (lambda (i)
                            (let ((a (vector-ref v i)))
                             (vector-set! v i (vector-ref temp a))))))))

    ;; Output
    (vector-ref temp output)))
         
)

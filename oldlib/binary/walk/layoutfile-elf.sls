(library (yuni binary walk layoutfile-elf)
         (export elf->layout)
         (import (rnrs)
                 (match)
                 (shorten)
                 (irregex)
                 (srfi :8)
                 (yuni core)
                 (yuni binary format elf0))

;;

(define irx-size (irregex '(: "QQ_M" ($ num) "_SIZEXX_" ($ (* any)))))
(define irx-obj (irregex '(: "QQ_M" ($ num) "_" ($ (* any)))))

(define (decompose-name str);; => basesym size? offset
  ;; FIXME: should be in genlayout?
  (define (try irx) ;; => num sym / #f #f
    (let ((m (irregex-match irx str)))
      (cond
        ((irregex-match-data? m)
         (values
           (string->symbol (irregex-match-substring m 2))
           (string->number (irregex-match-substring m 1))))
        (else
          (values #f #f)))))
  (receive (sym num) (try irx-size) ;; NB: SIZE should be first (longer)
    (if num
      (values sym #t num)
      (receive (sym num) (try irx-obj)
        (if num
          (values sym #f num)
          (values #f #f #f))))))

(define (elf->layout bv) ;; => ((sym offset size) ...)
  (define syms (elf64le-symbol-list bv))
  (define entries (make-eq-hashtable)) ;; ht :: sym => (#offset . #size)
  (define (fill value offset word)
    ;(display (list 'fill: value offset word))(newline)
    (let* ((realword (- word 4096))
           (shift (expt 2 (* offset 16)))
           (count (* realword shift)))
      (+ value count)))
  (define (enter e)
    (match e
           ((str info other value size)
            (receive (basesym size? num) (decompose-name str)
              (when basesym ;; ignore unknown formatted symbol
                (let ((p (hashtable-ref entries basesym #f)))
                  (cond
                    (p
                      (cond
                        (size?  (hashtable-set! 
                                  entries
                                  basesym
                                  (cons (car p)
                                        (fill (cdr p) num size))))
                        (else (hashtable-set!
                                entries
                                basesym
                                (cons (fill (car p) num size)
                                      (cdr p))))))
                    (else 
                      (hashtable-set!
                        entries
                        basesym
                        (cons (or (and (not size?)
                                       (fill 0 num size))
                                  0)
                              (or (and size?
                                       (fill 0 num size))
                                  0)))))))))))
  (for-each enter syms)
  (receive (key* value*) (hashtable-entries entries)
    (map (^[k v] (list k (car v) (cdr v)))
         (vector->list key*)
         (vector->list value*))))

)

(library (r7c-yunicore yuniport)
         (export
           make-yuniport
           yuniport?
           ;; Static
           yuniport-binary-port?
           yuniport-textual-port?
           yuniport-buffer-port?
           yuniport-input-port?
           yuniport-output-port?
           ;; Control
           yuniport-flush
           yuniport-input-port-open?
           yuniport-output-port-open?
           yuniport-close
           yuniport-close-input-port
           yuniport-close-output-port
           yuniport-get-buffer
           ;; Textual
           yuniport-read-char
           yuniport-peek-char
           yuniport-read-line
           yuniport-write-char
           yuniport-read-string
           yuniport-write-string
           ;; Binary
           yuniport-read-u8
           yuniport-peek-u8
           yuniport-write-u8
           yuniport-read-bytevector!
           yuniport-write-bytevector
           )
         (import (r7c-basic syntax define)
                 (r7c syntax and)
                 (r7c syntax cond)
                 (r7c syntax unless)
                 (r7c-yunicore simple-struct)
                 (r7c-system core)
                 (r7c heap fixnum)
                 (r7c heap pair))

(define yuniport-class (list '<yuniport>))

(define (yuniport? obj)
  (and (simple-struct? obj)
       (eq? yuniport-class (simple-struct-name obj))))

(define (%yuniport-method p idx)
  (unless (yuniport? p)
    (error "Port required"))
  (let ((x (simple-struct-ref p idx)))
   (if x
     x
     (cond
       ;; Specially handle the first 5 boolean fields
       (($fx< idx 5)
        x)
       (else
         (error "Port method unavailable" idx))))))

;; Static
(define (yuniport-textual-port? p)     (%yuniport-method p 0))
(define (yuniport-binary-port? p)      (%yuniport-method p 1))
(define (yuniport-buffer-port? p)      (%yuniport-method p 2))
(define (yuniport-input-port? p)       (%yuniport-method p 3))
(define (yuniport-output-port? p)      (%yuniport-method p 4))
;; Control
(define (yuniport-flush p)             ((%yuniport-method p 5)))
(define (yuniport-input-port-open? p)  ((%yuniport-method p 6)))
(define (yuniport-output-port-open? p) ((%yuniport-method p 7)))
(define (yuniport-close p)             ((%yuniport-method p 8)))
(define (yuniport-close-input-port p)  ((%yuniport-method p 9)))
(define (yuniport-close-output-port p) ((%yuniport-method p 10)))
(define (yuniport-get-buffer p)        ((%yuniport-method p 11)))

;; Textual
(define (yuniport-read-char p)         ((%yuniport-method p 12)))
(define (yuniport-peek-char p)         ((%yuniport-method p 13)))
(define (yuniport-read-line p)         ((%yuniport-method p 14)))
(define (yuniport-read-string p k)     ((%yuniport-method p 15) k))
(define (yuniport-write-char p c)      ((%yuniport-method p 16) c))
(define (yuniport-write-string p str start end)
                                       ((%yuniport-method p 17) str start end))

;; Binary
(define (yuniport-read-u8 p)           ((%yuniport-method p 18)))
(define (yuniport-peek-u8 p)           ((%yuniport-method p 19)))
(define (yuniport-read-bytevector! p bv start end)
                                       ((%yuniport-method p 20) bv start end))
(define (yuniport-write-u8 p c)        ((%yuniport-method p 21) c))
(define (yuniport-write-bytevector p bv start end)
                                       ((%yuniport-method p 22) bv start end))

(define (make-yuniport query)
  (make-simple-struct yuniport-class
                      23
                      (list
                        (query 'textual-port?)      ;; 0
                        (query 'binary-port?)       ;; 1
                        (query 'buffer-port?)       ;; 2
                        (query 'input-port?)        ;; 3
                        (query 'output-port?)       ;; 4
                        (query 'flush)              ;; 5
                        (query 'input-port-open?)   ;; 6
                        (query 'output-port-open?)  ;; 7
                        (query 'close)              ;; 8
                        (query 'close-input-port)   ;; 9
                        (query 'close-output-port)  ;; 10
                        (query 'get-buffer)         ;; 11
                        (query 'read-char)          ;; 12
                        (query 'peek-char)          ;; 13
                        (query 'read-line)          ;; 14
                        (query 'read-string)        ;; 15
                        (query 'write-char)         ;; 16
                        (query 'write-string)       ;; 17
                        (query 'read-u8)            ;; 18
                        (query 'peek-u8)            ;; 19
                        (query 'read-bytevector!)   ;; 20
                        (query 'write-u8)           ;; 21
                        (query 'write-bytevector)   ;; 22
                        )))
         
)

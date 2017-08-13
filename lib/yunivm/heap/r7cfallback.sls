(library (yunivm heap r7cfallback)
         (export make-r7cfallback)
         (import (yuni scheme)
                 (yunivm heap hostbridge)
                 (yunivm util compatlibs)
                 (yunivm util basiclibs))

(define (special-callback-type sym)
  ;; FIXME: This is incomplete! We have more
  ;;        callback-taking procedures such as 
  ;;        member, assoc, with-exception-handler...
  (case sym
    ((map string-map vector-map)
     'map-like)
    ((for-each string-for-each vector-for-each)
     'for-each-like)
    ;; make-vector can return vectors contains #<unspecified>
    ((make-vector) 'make-vector)
    ;; filehandle-read! passes mutable bytevector buffer 
    ((filehandle-read!) 'writeback1-2)
    (else #f)))
(define (zero-valued? sym)
  (memv sym basiclibs-zero-values))
(define (func2? sym)
  (case sym
    (($fl-floor/ $fl-truncate/ $fx-floor/ $fx-truncate/)
     #t)
    (else #f)))

(define basiclib/proc (vector->list basiclibs-proc-vector))
(define basiclib/name (vector->list basiclibs-name-vector))
(define compatlib/proc (vector->list compatlibs-proc-vector))
(define compatlib/name (vector->list compatlibs-name-vector))

(define make-vector/initf
  (case-lambda
    ((k) (make-vector k 'r7cf-unspec))
    ((k init) (make-vector k init))))

(define (make-r7cfallback coreops)
  (define co-unspecified (coreops 'unspecified))
  (define bv-set! (coreops 'bytevector-u8-set!))
  (define hostbridge (make-hostbridge coreops))
  (define host (hostbridge 'HOST))
  (define target (hostbridge 'TARGET))

  ;; Converters
  ; func0 (no return value)
  (define (func0 proc)
    (lambda args
      (let ((objs (map host args)))
       ;(write (list 'Call0: proc objs)) (newline)
       (apply proc objs)
       (co-unspecified))))

  ; func1 (1 return value)
  (define (func1 proc)
    (lambda args
      ;(write (list 'Call1: proc args)) (newline)
      (let ((objs (map host args)))
       (let ((r (apply proc objs)))
        ;(write (list 'Call1: proc objs '=> r)) (newline)
        (target r)))))

  ; writeback1 (1 return value, writeback arg2)
  (define (writeback1-2 proc)
    (lambda args
      (let ((objs (map host args)))
       (let ((r (apply proc objs)))
        ;(write (list 'Call1: proc objs '=> r)) (newline)
        (let* ((hostbv (cadr objs))
               (len (bytevector-length hostbv))
               (targetbv (cadr args)))
          (let loop ((idx 0))
           (unless (= idx len)
             (let ((b (bytevector-u8-ref hostbv idx)))
              (bv-set! targetbv idx b))
             (loop (+ idx 1)))))
        (target r)))))

  ; func2 (2 return value)
  (define (func2 proc)
    (lambda args
      (let ((objs (map host args)))
       (call-with-values (lambda () (apply proc objs))
                         (lambda (a b)
                           ;(write (list 'Call2: proc objs '=> a b)) (newline)
                           (values (target a) (target b)))))))
  ; rfunc1 (callback: 1 return value)
  (define (rfunc1 proc)
    (lambda args
      (let ((objs (map target args)))
       (let ((r (apply proc objs)))
        (host r)))))
  ; rfunc0 (callback: 0 return value)
  (define (rfunc0 proc)
    (lambda args
      (let ((objs (map target args)))
       (apply proc objs)
       #f)))
  ; map-like ^(cb objs ...)
  (define (map-like proc)
    (lambda (cb . args)
      (let ((objs (map host args))
            (xcb (rfunc1 cb)))
        (let ((r (apply proc xcb objs)))
         (target r)))))
  ; for-each-like ^(cb objs ...)
  (define (for-each-like proc)
    (lambda (cb . args)
      (let ((objs (map host args))
            (xcb (rfunc0 cb)))
        (apply proc xcb objs)
        #f)))

  (define fallbackalist
    (map
      (lambda (name proc)
        (cons
          name
          (let ((callback-type (special-callback-type name)))
           (case callback-type
             ((map-like)
              (map-like proc))
             ((for-each-like)
              (for-each-like proc))
             ((make-vector)
              (func1 make-vector/initf))
             ((writeback1-2)
              (writeback1-2 proc))
             (else
               (cond
                 ((zero-valued? name) (func0 proc))
                 ((func2? name) (func2 proc))
                 (else (func1 proc))))))))
      (append basiclib/name compatlib/name)
      (append basiclib/proc compatlib/proc)))
  (define (query sym)
    (let ((p (assq sym fallbackalist)))
     (unless p
       (error "Unknown symbol" sym))
     (cdr p)))

  query)
         
)

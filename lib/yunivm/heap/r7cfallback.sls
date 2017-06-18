(library (yunivm heap r7cfallback)
         (export make-r7cfallback)
         (import (yuni scheme)
                 (yunivm heap hostbridge)
                 (yunivm util basiclibs))

(define (special-callback-type sym)
  ;; FIXME: This is incomplete! We have more
  ;;        callback-taking procedures such as 
  ;;        member, assoc, with-exception-handler...
  (case sym
    ((map string-map vector-map
          for-each string-for-each vector-for-each)
     'map-like)
    (else #f)))
(define (zero-valued? sym)
  (memv sym basiclibs-zero-values))

(define basiclib/proc (vector->list basiclibs-proc-vector))
(define basiclib/name (vector->list basiclibs-name-vector))

(define (make-r7cfallback coreops)
  (define co-unspecified (coreops 'unspecified))
  (define hostbridge (make-hostbridge coreops))
  (define host (hostbridge 'HOST))
  (define target (hostbridge 'TARGET))

  ;; Converters
  ; func0 (no return value)
  (define (func0 proc)
    (lambda args
      (let ((objs (map host args)))
       (apply proc objs)
       (co-unspecified))))

  ; func1 (1 return value)
  (define (func1 proc)
    (lambda args
      (let ((objs (map host args)))
       (let ((r (apply proc objs)))
        (target r)))))
  ; rfunc1 (callback: 1 return value)
  (define (rfunc1 proc)
    (lambda args
      (let ((objs (map target args)))
       (let ((r (apply proc objs)))
        (host r)))))
  ; map-like ^(cb objs ...)
  (define (map-like proc)
    (lambda (cb . args)
      (let ((objs (map host args))
            (xcb (rfunc1 cb)))
        (let ((r (apply proc xcb objs)))
         (target r)))))

  (define fallbackalist
    (map
      (lambda (name proc)
        (cons
          name
          (let ((callback-type (special-callback-type name)))
           (case callback-type
             ((map-like)
              (map-like proc))
             (else
               (cond
                 ((zero-valued? name) (func0 proc))
                 (else (func1 proc))))))))
      basiclib/name
      basiclib/proc))
  (define (query sym)
    (let ((p (assq sym fallbackalist)))
     (unless p
       (error "Unknown symbol" sym))
     (cdr p)))

  query)
         
)

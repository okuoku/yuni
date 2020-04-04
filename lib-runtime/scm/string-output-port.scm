(define %ht-string-ports (make-eq-hashtable))

(define (open-output-string)
  (let* ((buf '())
         (p (make-soft-port 
              (vector
                (lambda (c) (set! buf (append buf (list c))))
                (lambda (s) (set! buf (append buf (string->list s))))
                (lambda () 'do-nothing)
                (lambda () (error "This is an output port"))
                (lambda () 'do-nothing))
              "w")))
    (hashtable-set! %ht-string-ports p
                    (lambda ()
                      (let ((s (list->string buf)))
                       (set! buf '())
                       s)))
    (add-finalizer
      p
      (lambda () (hashtable-delete! %ht-string-ports p)))
    p))

(define (get-output-string p)
  (let ((t (hashtable-ref %ht-string-ports p)))
   (t)))

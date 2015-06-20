(library (yuni ffi runtime simplepatcher)
         (export define-simplepatcher)
         (import (yuni scheme))

(define-syntax define-simplepatcher
  (syntax-rules ()
    ((_ name call bytevector->pointer set-pointer! offset-pointer)
     (define (name func in in-offset in-size out out-offset out-size
                   bv-v params*)
       (define (do-patch! dest-bv dest-offset src-bv src-offset)
         (let ((dest (if (= dest-bv -1) in (vector-ref bv-v dest-bv)))
               (off (if (= dest-bv -1) 
                      (+ (* 8 in-offset) dest-offset)
                      dest-offset))
               (src (vector-ref bv-v src-bv)))
           (set-pointer!
             dest
             off
             (if (bytevector? src)
               (bytevector->pointer src src-offset)
               (offset-pointer src src-offset)))))
       (define (itr cur)
         (when (pair? cur)
           (let ((a (car cur))
                 (next (cdr cur)))
             ;; 
             ;; a ::= (dest-offset src-bv . src-offset) |
             ;;       ((dest-bv . dest-offset) . (src-bv . src-offset))
             (let ((aa (car a))
                   (src-bv (cadr a))
                   (src-offset (cddr a)))
              (cond 
                ((pair? aa)
                 (let ((dest-bv (car aa))
                       (dest-offset (cdr aa)))
                   (do-patch! dest-bv dest-offset src-bv src-offset)))
                (else
                  (let ((dest-offset aa))
                    (do-patch! -1 dest-offset src-bv src-offset)))))
             (itr next))))
       (itr params*)
       (call func in in-offset in-size out out-offset out-size)))))
)

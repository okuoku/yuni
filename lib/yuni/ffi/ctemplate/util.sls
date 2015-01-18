;; Port output utility. FIXME: Move this as public library

(library (yuni ffi ctemplate util)
         (export
           put-obj)
         (import (yuni scheme))

(define (put-obj port . objs)
  (for-each (lambda (e) (display e port)) objs))

)

;; FIXME: Currently, nmosh only
(library (yunife build-id)
         (export build-id)
         (import (rnrs)
                 (primitives ex:unique-token))

(define current-build-id #f)

(define (build-id)
  (if current-build-id
    current-build-id
    (begin
      (set! current-build-id (ex:unique-token))
      current-build-id)))
         
)

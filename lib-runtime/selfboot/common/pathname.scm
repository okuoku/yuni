(define (%selfboot-match-ext? ext fn)
  (let ((len (string-length fn)))
   (and (< 3 len)
        (string=? ext (stubstring fn (- len 4) len)))))

(define (%selfboot-is-sls? fn) (%selfboot-match-ext? ".sls" fn))



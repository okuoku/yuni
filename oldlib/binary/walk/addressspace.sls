(library (yuni binary walk addressspace)
         (export
           aspace-new
           aspace-load
           aspace-describe
           aspace-make-index)
         (import (rnrs)
                 (yuni util search aa-tree)
                 (shorten)
                 (match)
                 (srfi :8)
                 (srfi :48)
                 (yuni core))

;; address space utilities

(define* aspace (library* symbols symbol-index))
(define (aspace-new)
  (make aspace 
        (library* '())
        (symbols '())
        (symbol-index #f)))

(define* (aspace-load (aspace) sym ranges offset)
  ;; FIXME: Use sym to allow unload
  (define (add-offset e)
    (let ((symbol (car e))
          (adr (cdr e)))
      (cons symbol (+ adr offset))))
  (~ aspace 'symbols := (append (~ aspace 'symbols) 
                                (map add-offset ranges))))

(define* (aspace-make-index (aspace))
  ;; Construct AA tree
  (let-with aspace (symbols)
    ;(write (list 'symbols: (length symbols)))(newline)
    (let* ((idx (aa-tree-init)) 
           (index (fold-left (^[cur e] 
                               (let ((next (aa-tree-insert cur (cdr e) (car e))))
                                 ;(write (list 'next: next))(newline)
                                 next
                                 )) 
                             idx
                             symbols)))
      ;(write (list 'index: index))(newline)
      (~ aspace 'symbol-index := index))))

(define* (aspace-describe (aspace) addr)
  (let-with aspace (symbol-index)
    (receive (la ls ra rs) (aa-tree-search-nearest symbol-index addr)
      (cond
        ((and la ls)
         (let ((diff (- addr la)))
           (format "~a+$~x" ls diff)))
        (else ;; no symbol...
          #f)))))

)

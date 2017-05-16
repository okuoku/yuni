(library (yunivm expander expandcore)
         (export 
           get-core-library0
           expand0)
         (import (yuni scheme)
                 (yuniexternal alexpander))

(define (get-core-library0) (yuniexternal-alexpander-get-init))
         
(define (expand0 frm)
  (let ((myenv (yuniexternal-alexpander-newenv)))
   (yuniexternal-alexpander-expand-top-level-forms!
     frm
     myenv)))
         
)

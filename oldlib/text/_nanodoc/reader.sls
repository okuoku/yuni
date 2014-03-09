(library (yuni text nanodoc reader)
         (export
           )
         (import (rnrs)
                 (srfi :14)
                 (yuni text peg))

;;
;; Document = Header* + "--.....\n" + Wiki area
;;

(define ($c* . obj) ($one-of (list->char-set obj)))
(define ($c*! . obj) ($none-of (list->char-set obj)))

(define eol ($or ($try ($seq cr lf)) cr lf))
(define ws ($skip-many ($c* #\space #\tab)))
(define ows ($optional ws))

(define name ($do (content ($many ($)))))

(define parensq-start ($c #\[))
(define parensq-end ($c #\]))

(define parensq ($or parensq-start parensq-end))

;; Data
(define datum ;; => string
  ($do (content ($many ($none-of parensq)))
       ($return (list->string content))))
(define datum-or-list
  ($or datum
       ($do parensq-start
            (content ($many datum-or-list))
            parensq-end
            ($return content))))

(define attribute ;; => string list
  ($do parensq-start
       (content datum-or-list)
       parensq-end
       ows
       ($return content)))

;; Special line sem
(define line-object
  ($do (($string "--"))
       ows
       (content ($optional attribute))
       ($return `(object-start ,content))))

(define block-start
  ($do (($string ">>"))
       ows
       (content ($optional attribute))
       ($return '(block-start ,content))))
;; NB: We have no "Block end" because it's just (string=? "<<")

)

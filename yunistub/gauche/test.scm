;;;
;;; Test yuniffi
;;;

(use gauche.test)

(test-start "yuniffi")
(use yuniffi)
(test-module 'yuniffi)

;; The following is a dummy test code.
;; Replace it for your tests.
(test* "test-yuniffi" "yuniffi is working"
       (test-yuniffi))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)





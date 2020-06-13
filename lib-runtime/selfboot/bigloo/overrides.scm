(define (%%yunierror msg irr)
  (error #f msg irr))

(yuni/base-library-add-var! '%%yunierror 'error)
(yuni/base-library-add-var! 'yuni/command-line 'command-line)


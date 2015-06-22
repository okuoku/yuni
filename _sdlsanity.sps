(import (yuni scheme)
        (yuniffi testing trivial-constants)
        (yuniffi graphics SDL2-constants))

(define-syntax disp
  (syntax-rules ()
    ((_ nam)
     (begin
       (display 'nam)
       (display ": ")
       (display nam)
       (newline)))))

(disp SDL_TRUE)
(disp SDL_FALSE)
(disp SDL_ICONV_ERROR)

(disp size_of_someopaque_t)

(disp REAL_1)
(disp REAL_2)

(flush-output-port (current-output-port))

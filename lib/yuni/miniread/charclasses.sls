(library (yuni miniread charclasses)
         (export
           ssplit-byte-delimiter?
           ssplit-byte-whitespace?
           ssplit-byte-class
           )
         (import (yuni scheme))

;; Return a character class symbol for a byte

(define (ssplit-byte-whitespace? byte)
  (case (ssplit-byte-class byte)
    ((SPACE TAB CR LF) #t)
    (else #f)))

(define (ssplit-byte-delimiter? byte)
  (or (ssplit-byte-whitespace? byte)
      (case (ssplit-byte-class byte)
        ((PAREN_L PAREN_R SQ_L SQ_R SEMICOLON SHARP DQUOTE) #t)
        (else #f))))

(define ssplit-byte-class/vector
  '#(;; 0
     #f
     ;; 1
     #f
     ;; 2
     #f
     ;; 3
     #f
     ;; 4
     #f
     ;; 5
     #f
     ;; 6
     #f
     ;; 7
     #f
     ;; 8
     #f
     ;; 9
     TAB
     ;; 10
     LF
     ;; 11
     #f
     ;; 12
     #f
     ;; 13
     CR
     ;; 14
     #f
     ;; 15
     #f
     ;; 16
     #f
     ;; 17
     #f
     ;; 18
     #f
     ;; 19
     #f
     ;; 20
     #f
     ;; 21
     #f
     ;; 22
     #f
     ;; 23
     #f
     ;; 24
     #f
     ;; 25
     #f
     ;; 26
     #f
     ;; 27
     #f
     ;; 28
     #f
     ;; 29
     #f
     ;; 20
     #f
     ;; 31
     #f
     ;; 32
     SPACE
     ;; 33
     #f
     ;; 34
     DQUOTE
     ;; 35
     SHARP
     ;; 36
     #f
     ;; 37
     #f
     ;; 38
     #f
     ;; 39
     FQUOTE
     ;; 40
     PAREN_L
     ;; 41
     PAREN_R
     ;; 42
     #f
     ;; 43
     #f
     ;; 44
     COMMA
     ;; 45
     #f
     ;; 46
     DOT
     ;; 47
     #f
     ;; 48
     #f
     ;; 49
     #f
     ;; 50
     #f
     ;; 51
     #f
     ;; 52
     #f
     ;; 53
     #f
     ;; 54
     #f
     ;; 55
     #f
     ;; 56
     #f
     ;; 57
     #f
     ;; 58
     #f
     ;; 59
     SEMICOLON
     ;; 60
     #f
     ;; 61
     #f
     ;; 62
     #f
     ;; 63
     #f
     ;; 64
     AT
     ;; 65
     #f
     ;; 66
     #f
     ;; 67
     #f
     ;; 68
     #f
     ;; 69
     #f
     ;; 70
     LARGE-F
     ;; 71
     #f
     ;; 72
     #f
     ;; 73
     #f
     ;; 74
     #f
     ;; 75
     #f
     ;; 76
     #f
     ;; 77
     #f
     ;; 78
     #f
     ;; 79
     #f
     ;; 80
     #f
     ;; 81
     #f
     ;; 82
     #f
     ;; 83
     #f
     ;; 84
     LARGE-T
     ;; 85
     #f
     ;; 86
     #f
     ;; 87
     #f
     ;; 88
     #f
     ;; 89
     #f
     ;; 90
     #f
     ;; 91
     SQ_L
     ;; 92
     BACKSLASH
     ;; 93
     SL_R
     ;; 94
     #f
     ;; 95
     #f
     ;; 96
     BQUOTE
     ;; 97
     #f
     ;; 98
     #f
     ;; 99
     #f
     ;; 100
     #f
     ;; 101
     #f
     ;; 102
     SMALL-F
     ;; 103
     #f
     ;; 104
     #f
     ;; 105
     #f
     ;; 106
     #f
     ;; 107
     #f
     ;; 108
     #f
     ;; 109
     #f
     ;; 110
     #f
     ;; 111
     #f
     ;; 112
     #f
     ;; 113
     #f
     ;; 114
     #f
     ;; 115
     #f
     ;; 116
     SMALL-T
     ;; 117
     #f
     ;; 118
     #f
     ;; 119
     #f
     ;; 120
     #f
     ;; 121
     #f
     ;; 122
     #f
     ;; 123
     #f
     ;; 124
     PIPE
     ;; 125
     #f
     ;; 126
     #f
     ;; 127
     #f
     ))

(define (ssplit-byte-class b)
  (and (< b 128)
       (vector-ref ssplit-byte-class/vector b)))
)

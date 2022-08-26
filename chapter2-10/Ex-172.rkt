#lang htdp/bsl

; List-of-strings is one of:
; - '()
; - (cons String List-of-strings)

(define ex0 '())
(define ex1 (cons "abc" '()))
(define ex2 (cons "xyz" (cons "def" '())))

; List-of-list-of-strings (abbr. LN) is one of:
; - '()
; - (cons List-of-strings List-of-list-of-strings)

(define ex10 '())
(define ex11 (cons ex1 (cons ex0 ex1)))
(define ex12 (cons ex1 (cons ex0 ex2)))

; LN -> String
; convert lines in lls to a string
(define (collapse lls)
  (cond [(empty? lls) ...]
        [else (... (first lls) ...
               ... (rest lls) ...)]))

(check-expect (collapse ex10) "")
(check-expect (collapse ex11) "abc\nabc")
(check-expect (collapse ex12) "abc\nxyzdef")


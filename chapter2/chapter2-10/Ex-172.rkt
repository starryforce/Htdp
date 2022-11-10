#lang htdp/bsl

(require 2htdp/batch-io)

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
(define ex11 (cons ex1 (cons ex0 (cons ex1 '()))))
(define ex12 (cons ex1 (cons ex0 (cons ex2 '()))))

; LN -> String
; convert lines in lls to a string,
; no " " at the end of one line
; and no "\n" at the end of the string
(define (collapse lls)
  (cond [(empty? lls) ""]
        [else (string-append (handle-line (first lls))
                             (if (empty? (rest lls)) "" "\n")
                             (collapse (rest lls)))]))

(check-expect (collapse ex10) "")
(check-expect (collapse ex11) "abc\n\nabc")
(check-expect (collapse ex12) "abc\n\nxyz def")

; List-of-strings -> String
; convert a list of strings to a string, seperate by " "
(define (handle-line los)
  (cond [(empty? los) ""]
        [else (string-append (first los)
                             (if (empty? (rest los)) "" " ")
                             (handle-line (rest los)))]))

(check-expect (handle-line ex0) "")
(check-expect (handle-line ex1) "abc")
(check-expect (handle-line ex2) "xyz def")

(write-file "ttt.dat"
            (collapse (read-words/line "ttt.txt")))


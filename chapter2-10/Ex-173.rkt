#lang htdp/bsl

(require 2htdp/batch-io)

; List-of-strings is one of:
; - '()
; - (cons String List-of-strings)

(define ex0 '())
(define ex1 (cons "abc" '()))
(define ex2 (cons "xyz" (cons "def" '())))
(define ex3 (cons "a" '()))
(define ex4 (cons "the" (cons "b" '())))
(define ex5 (cons "an" (cons "b" (cons "a" '()))))


; List-of-strings -> String
; convert a list of strings to a string, seperate by " "
(define (handle-line los)
  (cond [(empty? los) ""]
        [else (cond [(articles? (first los)) (handle-line (rest los))]
                    [else (string-append (first los)
                             (if (or (empty? (rest los)) (articles? (first (rest los))))  "" " ")
                             (handle-line (rest los)))])]))

(check-expect (handle-line ex0) "")
(check-expect (handle-line ex1) "abc")
(check-expect (handle-line ex2) "xyz def")
(check-expect (handle-line ex3) "")
(check-expect (handle-line ex4) "b")
(check-expect (handle-line ex5) "b")

; String -> Boolean
; determine if s is an article
(define (articles? s)
  (or (string=? s "a")
      (string=? s "an")
      (string=? s "the")))

(check-expect (articles? "a") #true)
(check-expect (articles? "an") #true)
(check-expect (articles? "the") #true)
(check-expect (articles? "c") #false)

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
(define (remove-articles lls)
  (cond [(empty? lls) ""]
        [else (string-append (handle-line (first lls))
                             (if (empty? (rest lls)) "" "\n")
                             (remove-articles (rest lls)))]))

;(check-expect (remove-articles ex10) "")
;(check-expect (remove-articles ex11) "abc\n\nabc")
;(check-expect (remove-articles ex12) "abc\n\nxyz def")



(define (remove-articles-file n)
  (write-file (string-append "no-articles-" n)
            (remove-articles (read-words/line n))))
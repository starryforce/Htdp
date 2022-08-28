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
(define (encode-text lls)
  (cond [(empty? lls) ""]
        [else (string-append (handle-line (first lls))
                             (if (empty? (rest lls)) "" "\n")
                             (encode-text (rest lls)))]))

(check-expect (encode-text ex10) "")
(check-expect (encode-text ex11) "097098099\n\n097098099")
(check-expect (encode-text ex12) "097098099\n\n120121122 100101102")

; List-of-strings -> String
; convert a list of strings to a string, seperate by " "
(define (handle-line los)
  (cond [(empty? los) ""]
        [else (string-append (encode-word (explode (first los)))
                             (if (empty? (rest los)) "" " ")
                             (handle-line (rest los)))]))

(check-expect (handle-line ex0) "")
(check-expect (handle-line ex1) "097098099")
(check-expect (handle-line ex2) "120121122 100101102")

; List-of-1String -> String
; transform every item in l to a numeric three-letter string with a value between 0 and 256
; and join them into a string
(define (encode-word w)
  (cond [(empty? w) ""]
        [else (string-append
               (encode-letter (first w))
               (encode-word (rest w)))]))


; 1String -> String
; converts the given 1String to a 3-letter numeric String
 
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
 
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))
 
; 1String -> String
; converts the given 1String into a String
 
(check-expect (code1 "z") "122")
 
(define (code1 c)
  (number->string (string->int c)))


(define (encode-file n)
  (write-file (string-append "encode-" n)
            (encode-text (read-words/line n))))



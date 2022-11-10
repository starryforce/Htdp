#lang htdp/bsl

(require 2htdp/batch-io)

(define-struct count [1string words lines])
; A Count is a structure:
; (make-count Number Number Number)
; interpretation (make-count s w l) represents the file
; has s 1String, w words, l lines

; List-of-strings is one of:
; - '()
; - (cons String List-of-strings)

(define ex0 '())
(define ex1 (cons "abc" '()))
(define ex2 (cons "xyz" (cons "def" '())))

; List-of-list-of-strings (abbr. LLS) is one of:
; - '()
; - (cons List-of-strings LLS)

(define ex10 '())
(define ex11 (cons ex1 (cons ex0 (cons ex1 '()))))
(define ex12 (cons ex1 (cons ex0 (cons ex2 '()))))

; LLS -> Count
(define (count-info lls)
  (cond [(empty? lls) (make-count 0 0 0)]
        [else (make-count (+ (count-1strings-quantity (first lls)) (count-1string (count-info (rest lls))))
                          (+ (count-words-quantity (first lls)) (count-words (count-info (rest lls))))
                          (+ 1 (count-lines (count-info (rest lls)))))]))

(check-expect (count-info ex10) (make-count 0 0 0))
(check-expect (count-info ex11) (make-count 6 2 3))
(check-expect (count-info ex12) (make-count 9 3 3))

; List-of-strings -> Number
; evaluate the number of 1strings
(define (count-1strings-quantity los)
  (cond [(empty? los) 0]
        [else (+ (length (explode (first los)))
                   (count-1strings-quantity (rest los)))]))

; List-of-strings -> Number
; evaluate number of words
(define (count-words-quantity los) (length los))


; String -> Count
; calc count info of file n
(define (wc n)
  (count-info (read-words/line n)))
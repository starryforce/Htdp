#lang htdp/bsl+

; A List-of-1strings is one of:
; - '()
; - (cons 1String List-of-1string)

; A Suffix is one of:
; - (cons String '())
; - (cons String Suffix)

; A List-of-suffixes is one of:
; - '()
; - (cons Suffix List-of-suffixes)
; constraint every item in List-of-suffixes is longer than last

(define ex1 (list "a" "b" "c"))
(define ex2 (list "a"))

; List-of-1strings -> List-of-suffixes
(define (suffixes alos)
  (cond [(empty? alos) '()]
        [else (cons (cons (first alos) (rest alos))
                    (suffixes (rest alos)))]))

(check-expect (suffixes '()) '())
(check-expect (suffixes ex1) (list (list "a" "b" "c") (list "b" "c") (list "c") ))
(check-expect (suffixes ex2) (list ex2))
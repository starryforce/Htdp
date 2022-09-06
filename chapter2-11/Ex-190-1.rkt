#lang htdp/bsl+

; A List-of-1strings is one of:
; - '()
; - (cons 1String List-of-1string)

; A Prefix is one of:
; - (cons String '())
; - (cons String Prefix)

; A List-of-prefixes is one of:
; - '()
; - (cons Prefix List-of-prefixes)
; constraint every item in List-of-prefixes is longer than last

(define ex1 (list "a" "b" "c"))

; List-of-1strings -> List-of-prefixes
(define (prefixes alos)
  (cond [(empty? alos) '()]
        [else (cons (list (first alos))
                    (attach (first alos) (prefixes (rest alos))))]))

(check-expect (prefixes ex1) (list (list "a") (list "a" "b") (list "a" "b" "c")))

; String List-of-prefixes -> List-of-prefixes
(define (attach s alop)
  (cond [(empty? alop) '()]
        [else (cons (cons s (first alop))
                    (attach s (rest alop)))]))


(check-expect (attach "a" '()) '())
(check-expect (attach "a" (list (list "b") (list "c"))) (list (list "a" "b") (list "a" "c")))
#lang htdp/bsl

(define EXCHANGE_RATE 1.1)

; Number -> Number;
; convert an amounts of US$ u to € amounts
(define (u2e u) (* u EXCHANGE_RATE))

(check-expect (u2e 0) 0)
(check-expect (u2e 1) 1.1)
(check-expect (u2e 20.47) 22.517)

(define ex0 '())
(define ex1 (cons 1 '()))
(define ex2 (cons 10.9 (cons 20.47 '())))

; List-of-numbers -> List-of-numbers
; converts a list of US$ amounts into a list of € amounts
(define (convert-euro alou)
  (cond [(empty? alou) '()]
        [else (cons (u2e (first alou)) (convert-euro (rest alou)))]))

(check-expect (convert-euro ex0) '())
(check-expect (convert-euro ex1) (cons 1.1 '()))
(check-expect (convert-euro ex2) (cons (* 10.9  EXCHANGE_RATE) (cons (* EXCHANGE_RATE 20.47) '())))


; Number Number -> Number;
; convert an amounts of US$ u to € amounts, convert rate is r
(define (u2e* r u) (* r u))

(check-expect (u2e* 0 10) 0)
(check-expect (u2e* 1 2.7) 2.7)
(check-expect (u2e* 20.47 1.1) 22.517)

; Number List-of-numbers -> List-of-numbers
; consumes an exchange rate r and a list of US$ amounts alou
; and converts the latter into a list of € amounts
(define (convert-euro* r alou)
  (cond [(empty? alou) '()]
        [else (cons (u2e* r (first alou)) (convert-euro (rest alou)))]))

(check-expect (convert-euro* 10.8 ex0) '())
(check-expect (convert-euro* 8.9 ex1) (cons 8.9 '()))
(check-expect (convert-euro* 1.1 ex2) (cons (* 10.9  EXCHANGE_RATE) (cons (* EXCHANGE_RATE 20.47) '())))
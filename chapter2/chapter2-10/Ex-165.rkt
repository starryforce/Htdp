#lang htdp/bsl

(define ex0 '())
(define ex1 (cons "abc0" '()))
(define ex2 (cons "robot" '()))
(define ex3 (cons "robot" (cons "abc" '())))
(define ex4 (cons "abc" (cons "efg" '())))
(define ex5 (cons "robot" (cons "robot" '())))

; List-of-strings -> List-of-strings
; replaces all occurrences of "robot" in alos with "r2d2"
(define (subst-robot alos)
  (cond [(empty? alos) '()]
        [else (cons (if (string=? (first alos) "robot") "r2d2" (first alos))
                    (subst-robot (rest alos)))]))

(check-expect (subst-robot ex0) '())
(check-expect (subst-robot ex1) (cons "abc0" '()))
(check-expect (subst-robot ex2) (cons "r2d2" '()))
(check-expect (subst-robot ex3) (cons "r2d2" (cons "abc" '())))
(check-expect (subst-robot ex4) (cons "abc" (cons "efg" '())))
(check-expect (subst-robot ex5) (cons "r2d2" (cons "r2d2" '())))


; String String List-of-strings -> List-of-strings
; replace all occurrences of old with new in alos
(define (substitute new old alos)
  (cond [(empty? alos) '()]
        [else (cons (if (string=? (first alos) old) new (first alos))
                    (substitute new old (rest alos)))] ))

(check-expect (substitute "r2d2" "robot" ex0)  '())
(check-expect (substitute "r2d2" "robot" ex1) (cons "abc0" '()))
(check-expect (substitute "r2d2" "robot" ex2) (cons "r2d2" '()))
(check-expect (substitute "r2d2" "robot" ex3) (cons "r2d2" (cons "abc" '())))
(check-expect (substitute "r2d2" "robot" ex4) (cons "abc" (cons "efg" '())))
(check-expect (substitute "r2d2" "robot" ex5) (cons "r2d2" (cons "r2d2" '())))
#lang htdp/isl+

(require 2htdp/abstraction)

; Number -> [List-of Number]
; tabulates sin between n 
; and 0 (incl.) in a list
(define (tab-sin n)
  (cond
    [(= n 0) (list (sin 0))]
    [else
     (cons
      (sin n)
      (tab-sin (sub1 n)))]))

; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
(define (tab-sqrt n)
  (cond
    [(= n 0) (list (sqrt 0))]
    [else
     (cons
      (sqrt n)
      (tab-sqrt (sub1 n)))]))


; Number -> [List-of Number]
(define (tabulate n f)
  (reverse (for/list ([i (add1 n)])
    (f i))))

(define (tab-sin-from-abstract n)
  (tabulate n sin))

(define delta 0.001)
(check-within (tab-sin-from-abstract 4) (tab-sin 4) delta)
(check-within (tab-sin-from-abstract 2) (tab-sin 2) delta)
(check-within (tab-sin-from-abstract 0) (tab-sin 0) delta)

(define (tab-sqrt-from-abstract n)
  (tabulate n sqrt))

(check-within (tab-sqrt-from-abstract 4) (tab-sqrt 4) delta)
(check-within (tab-sqrt-from-abstract 2) (tab-sqrt 2) delta)
(check-within (tab-sqrt-from-abstract 0) (tab-sqrt 0) delta)


(define (tab-sqr-from-abstract n)
  (tabulate n sqr))

(define (tab-tan-from-abstract n)
  (tabulate n tan))

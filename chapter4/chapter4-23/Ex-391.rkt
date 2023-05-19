#lang htdp/isl+

#| Exercise 391.
Design replace-eol-with using the strategy of Processing Two Lists Simultaneously: Case 3.
Start from the tests. Simplify the result systematically.
|#

; [List-of Number] [List-of Number] -> [List-of Number]
; replaces the final '() in front with end
(define (replace-eol-with front end)
  (cond [(empty? front) end]
        [(empty? end) front]
        [(cons? end)
         (cons (first front)
               (replace-eol-with (rest front) end))]))

(check-expect (replace-eol-with (cons 1 '()) (cons 'a '())) (cons 1 (cons 'a '())))
; (replace-eol-with front (rest end)) ; (cons 1 '())
; (replace-eol-with (rest front) end)) ; (cons 'a '())
; (replace-eol-with (rest front) (rest end)) ;  '()


(check-expect (replace-eol-with '() '()) '())
(check-expect (replace-eol-with '() '(a b)) '(a b))
(check-expect (replace-eol-with (cons 1 '()) '(a))
              (cons 1 '(a)))
(check-expect (replace-eol-with
               (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))
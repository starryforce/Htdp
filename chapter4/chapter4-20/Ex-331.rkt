#lang htdp/isl+

; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a String.

(define Text (cons "part1" (cons "part2" (cons "part3" '()))))
(define Code (cons "hang" (cons "draw" '())))
(define Docs (cons "read!" '()))
(define Libs (cons Code (cons Docs '())))
(define TS (cons Text (cons "read!" (cons Libs '()))))


; Dir.v1 - > Number
; determines how many files dir contains
(define (how-many dir)
  (cond [(empty? dir) 0]
        [else (+ (cond [(string? (first dir)) 1]
                       [else (how-many (first dir))])
                 (how-many (rest dir)))]))


(check-expect (how-many Text) 3)
(check-expect (how-many Code) 2)
(check-expect (how-many Docs) 1)
(check-expect (how-many Libs) 3)
(check-expect (how-many TS) 7)
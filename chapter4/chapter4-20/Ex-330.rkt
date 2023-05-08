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
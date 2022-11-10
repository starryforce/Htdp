#lang htdp/bsl

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S) 
; An Lo1S is one of: 
; – '()
; – (cons 1String Lo1S)

(define good
  (cons "g" (cons "o" (cons "o" (cons "d" '())))))
(define lla
  (cons "l" (cons "l" (cons "a" '()))))

; String String -> Editor
; generate an Editor from left in pre,
; and right in post in the form of list
(define (create-editor left right)
  (make-editor (reverse (explode left)) (explode right)))

(check-expect (create-editor "" "good") (make-editor '() good))
(check-expect (create-editor "all" "") (make-editor lla '()))
(check-expect (create-editor "all" "good") (make-editor lla good))
(check-expect (create-editor "a" "g") (make-editor (cons "a" '()) (cons "g" '())))


              
              


#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

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

(define HEIGHT 20) ; the height of the editor 
(define WIDTH 200) ; its width 
(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 
 
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

; Editor -> Image
; renders an editor as an image of the two texts 
; separated by the cursor 
(define (editor-render e) MT)
 
; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(define (editor-kh ed ke) ed)

; main : String -> Editor
; launches the editor given some initial string 
(define (main s)
   (big-bang (create-editor s "")
     [on-key editor-kh]
     [to-draw editor-render]))

; - when the editor is empty
; - when the cursor is at the left end
; - or right end of the non-empty string in the editor
; -  and when it is in the middle

(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect (editor-kh (create-editor "cd" "fgh") "e")
              (create-editor "cde" "fgh"))
; case "\b"
(check-expect (editor-kh (create-editor "" "") "\b")
              (create-editor "" ""))
(check-expect (editor-kh (create-editor "abc" "") "\b")
              (create-editor "ab" ""))
(check-expect (editor-kh (create-editor "" "abc") "\b")
              (create-editor "" "abc"))
(check-expect (editor-kh (create-editor "abc" "def") "\b")
              (create-editor "ab" "def"))

; case "left"
(check-expect (editor-kh (create-editor "" "") "left")
              (create-editor "" ""))
(check-expect (editor-kh (create-editor "abc" "") "left")
              (create-editor "ab" "c"))
(check-expect (editor-kh (create-editor "" "abc") "left")
              (create-editor "" "abc"))
(check-expect (editor-kh (create-editor "abc" "def") "left")
              (create-editor "ab" "cdef"))

; case "right"
(check-expect (editor-kh (create-editor "" "") "right")
              (create-editor "" ""))
(check-expect (editor-kh (create-editor "abc" "") "right")
              (create-editor "abc" ""))
(check-expect (editor-kh (create-editor "" "abc") "right")
              (create-editor "a" "bc"))
(check-expect (editor-kh (create-editor "abc" "def") "right")
              (create-editor "abcd" "ef"))

(check-expect (editor-kh (create-editor "abc" "def") "up")
              (create-editor "abc" "def"))
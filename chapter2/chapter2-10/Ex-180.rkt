#lang htdp/bsl

(require 2htdp/image)

(define HEIGHT 20) ; the height of the editor 
(define WIDTH 200) ; its width 
(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 
 
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))


(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S) 
; An Lo1S is one of: 
; – '()
; – (cons 1String Lo1S)

; Lo1s -> Image
; renders a list of 1Strings as a text image
(define (editor-text s)
  (text (join-str s) FONT-SIZE FONT-COLOR))

(define (join-str s)
  (cond
    [(empty? s) ""]
    [else (string-append (first s) (join-str (rest s)))]))


(check-expect
  (editor-text
   (cons "p" (cons "o" (cons "s" (cons "t" '())))))
  (text "post" FONT-SIZE FONT-COLOR))
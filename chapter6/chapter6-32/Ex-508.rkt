#lang htdp/isl+

(require 2htdp/image)

(define FONT-SIZE 11)
(define FONT-COLOR "black")
 
; [List-of 1String] -> Image
; renders a string as an image for the editor 
(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))
 
(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor [List-of 1String] [List-of 1String])
; interpretation if (make-editor p s) is the state of 
; an interactive editor, (reverse p) corresponds to
; the text to the left of the cursor and s to the
; text on the right

; [List-of 1String] N -> Editor
; produce an Editor satified
; (1) p and s make up ed and
; (2) x is larger than the image of p and smaller than
; the image of p extended with the first 1String on s (if any).
; (image-width (editor-text '()))            ==  0
; (image-width (editor-text '("l")))         ==  2
; (image-width (editor-text '("l" "m")))     == 10
; (image-width (editor-text '("l" "m" "n"))) == 16
(check-expect (split-structural '() 10) (make-editor '() '()))
(check-expect (split-structural '("l" "m" "n") 0) (make-editor '() '("l" "m" "n")))
(check-expect (split-structural '("l" "m" "n") 1) (make-editor '() '("l" "m" "n"))) 
(check-expect (split-structural '("l" "m" "n") 2) (make-editor '("l") '("m" "n")))
(check-expect (split-structural '("l" "m" "n") 6) (make-editor '("l") '("m" "n")))
(check-expect (split-structural '("l" "m" "n") 10) (make-editor '("l" "m") '("n")))
(check-expect (split-structural '("l" "m" "n") 16) (make-editor '("l" "m" "n") '()))
(check-expect (split-structural '("l" "m" "n") 17) (make-editor '("l" "m" "n") '()))
(define (split-structural ed x)
  (local (; [List-of 1String] [List-of 1String] -> Editor
          (define (split pre post)
            (cond  [(meet? pre post x) (make-editor pre post)]
                   [else (split (append pre (list (first post))) (rest post))])))
    (split '() ed)))

; [List-of 1String] [List-of 1String] N -> Boolean
; determine if pre & post match x coordinate
(check-expect (meet? '() '("l" "m" "n") 0) #t)
(check-expect (meet? '() '("l" "m" "n") 1) #t)
(check-expect (meet? '("l") '("m" "n") 2) #t)
(check-expect (meet? '("l" "m" "n") '() 2) #f)
(check-expect (meet? '("l") '("m" "n") 6) #t)
(check-expect (meet? '("l" "m") '("n") 10) #t)
(check-expect (meet? '("l" "m" "n") '() 10) #f)
(check-expect (meet? '("l" "m" "n") '() 16) #t)
(check-expect (meet? '("l" "m" "n") '() 17) #t)

(define (meet? pre post x)
  (cond [(cons? post) (and (<= (image-width (editor-text pre)) x)
                           (< x (image-width (editor-text (append pre (list (first post)))))))]
        [else (<= (image-width (editor-text pre)) x)]))

(define (meet?.v2 pre post x)
  (and (<= (image-width (editor-text pre)) x)
       (or (empty? post)
           (< x (image-width (editor-text (append pre (list (first post)))))))))
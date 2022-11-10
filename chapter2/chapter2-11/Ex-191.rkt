#lang htdp/bsl+

(require 2htdp/image)

(define triangle-p
  (list
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 30 20)))

(define square-p
  (list
    (make-posn 10 10)
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 10 20)))



; a plain background image 
(define MT (empty-scene 50 50))

; An NELoP is one of: 
; – (cons Posn '())
; – (cons Posn NELoP)


; Image NELoP -> Image 
; connects the dots in p by rendering lines in img
(define (connect-dots img p)
  MT)

(check-expect (connect-dots MT triangle-p)
              (scene+line
               (scene+line MT 20 20 30 20 "red")
               20 10 20 20 "red"))

(check-expect (connect-dots MT square-p)
              (scene+line
               (scene+line
                (scene+line MT 10 10 20 10 "red")
                20 10 20 20 "red")
               20 20 10 20 "red"))
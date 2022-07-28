#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

; ExpectsToSee is one of:
; – AA
; – BB
; – DD 
; – ER 
 
(define AA "start, expect an 'a'")
(define BB "expect 'b', 'c', or 'd'")
(define DD "finished")
(define ER "error, illegal key")

; ExpectsToSee -> Image
; render the image according to the state
(define (render e) (cond [(string=? e AA) (rectangle 100 100 "solid" "white")]
                         [(string=? e BB) (rectangle 100 100 "solid" "yellow")]
                         [(string=? e DD) (rectangle 100 100 "solid" "green")]
                         [(string=? e ER) (rectangle 100 100 "solid" "red")]))

(check-expect (render AA) (rectangle 100 100 "solid" "white"))
(check-expect (render BB) (rectangle 100 100 "solid" "yellow"))
(check-expect (render DD) (rectangle 100 100 "solid" "green"))
(check-expect (render ER) (rectangle 100 100 "solid" "red"))

; ExpectsToSee -> ExpectsToSee
; change state when press particular key
(define (input e ke) (cond [(string=? e AA) (cond [(key=? ke "a") BB]
                                                  [else e])]
                           [(string=? e BB) (cond [(or (key=? ke "b") (key=? ke "c")) e]
                                                  [(key=? ke "d") DD]
                                                  [else ER])]
                           [else e]))
                           

(check-expect (input AA "a") BB)
(check-expect (input AA "f") AA)
(check-expect (input BB "b") BB)
(check-expect (input BB "c") BB)
(check-expect (input BB "d") DD)
(check-expect (input BB "a") ER)

(define main
  (big-bang AA
    [on-draw render]
    [on-key input]))
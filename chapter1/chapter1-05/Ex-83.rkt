#lang htdp/bsl

(require 2htdp/image)

(define SCENE (empty-scene 200 20))
(define CURSOR (rectangle 1 20 "solid" "red"))

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t
(define ex1 (make-editor "hello," "world"))
(define ex2 (make-editor "start" ""))
(define ex3 (make-editor "" "end"))

; Editor -> Image
; render the state of e to the screen
(define (render e) (overlay/align "left" "center"
               (beside (text (editor-pre e) 16 "black") CURSOR (text (editor-post e) 16 "black"))
               SCENE))

(check-expect (render ex1) (overlay/align "left" "center"
               (beside (text "hello," 16 "black") CURSOR (text "world" 16 "black"))
               SCENE))

(check-expect (render ex2) (overlay/align "left" "center"
               (beside (text "start" 16 "black") CURSOR)
               SCENE))

(check-expect (render ex3) (overlay/align "left" "center"
               (beside CURSOR (text "end" 16 "black"))
               SCENE))
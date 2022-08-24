#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

(define CURSOR (rectangle 1 20 "solid" "red"))
(define SCENE (empty-scene 200 20))

(define-struct editor [text cursor])
; An Editor is a structure:
; (make-editor String Number)
; interpretation (make-editor s c) describes an editor
; whose visible text is text with the cursor displayed
; between the cursor and (+ cursor 1)
(define ex1 (make-editor "hello" 0))
(define ex2 (make-editor "hello" 1))
(define ex3 (make-editor "hello" 5))
(define ex4 (make-editor "" 0))

; Editor -> Image
; generate a view with text (ed-text ed) and cursor is
; between (ed-cursor ed) and (+ (ed-cursor ed) 1)
(define (render ed) (overlay/align "left" "center"
               (beside (text (substring (editor-text ed) 0 (editor-cursor ed)) 16 "black")
                       CURSOR
                       (text (substring (editor-text ed) (editor-cursor ed) (string-length (editor-text ed))) 16 "black"))
               SCENE))
(check-expect (render ex1) (overlay/align "left" "center"
               (beside CURSOR (text "hello" 16 "black"))
               SCENE))
(check-expect (render ex2) (overlay/align "left" "center"
               (beside (text "h" 16 "black") CURSOR (text "ello" 16 "black"))
               SCENE))
(check-expect (render ex3) (overlay/align "left" "center"
               (beside (text "hello" 16 "black") CURSOR)
               SCENE))
(check-expect (render ex4) (overlay/align "left" "center" CURSOR SCENE))

; Editor KeyEvent -> Editor
; generate a new Editor according to the origin Editor and KeyEvent
; A KeyEvent is one of the following:
; - 1String
;   - "\b"
;   - "\t"
;   - "\r"
;   - others
; - "left"
; - "right"
; - others
(define (edit ed ke) (cond
                        [(= (string-length ke) 1)
                         (cond
                           [(string=? ke "\b") (delete ed)]
                           [(> (string-length (editor-text ed)) 20) ed]
                           [(or (string=? ke "\t") (string=? ke "\r")) ed]
                           [else (insert ed ke)])]
                        [(or (string=? "left" ke) (string=? "right" ke)) (move-cursor ed ke)]
                        [else ed]))


(check-expect (edit ex1 "\b") ex1)
(check-expect (edit ex4 "\b") ex4)
(check-expect (edit ex2 "\b") (make-editor "ello" 0))

(check-expect (edit ex2 "\t") ex2)
(check-expect (edit ex2 "\r") ex2)

(check-expect (edit ex2 "w") (make-editor "hwello" 2))
(check-expect (edit ex3 "w") (make-editor "hellow" 6))

(check-expect (edit ex1 "left") ex1)
(check-expect (edit ex2 "left") ex1)
(check-expect (edit ex4 "left") ex4)

(check-expect (edit ex1 "right") ex2)
(check-expect (edit ex3 "right") ex3)
(check-expect (edit ex4 "right") ex4)

(check-expect (edit ex2 "up") ex2)
(check-expect (edit ex4 "up") ex4)


; Editor -> Editor
; handle the press of the delete,
; say "\b"
(define (delete ed) (make-editor
                     (string-append
                      (string-remove-last (substring (editor-text ed) 0 (editor-cursor ed)))
                      (substring (editor-text ed) (editor-cursor ed) (string-length (editor-text ed))))
                     (if
                      (< (- (editor-cursor ed) 1) 0)
                      0
                      (- (editor-cursor ed) 1))))

; Editor KeyEvent -> Editor
; handle the press of 1String key press,
; e.g. "w" "s" "j" "1"
(define (insert ed ke) (make-editor
                        (string-append
                         (substring (editor-text ed) 0 (editor-cursor ed))
                         ke
                         (substring (editor-text ed) (editor-cursor ed) (string-length (editor-text ed))))
                        (+ (editor-cursor ed) 1)))

(define (move-cursor ed ke) (cond
                              [(string=? ke "left") (make-editor (editor-text ed)
                                                                 (if (< (- (editor-cursor ed) 1) 0)
                                                                     0
                                                                     (- (editor-cursor ed) 1)))]
                              [(string=? ke "right") (make-editor (editor-text ed)
                                                                  (if (> (+ (editor-cursor ed) 1) (string-length (editor-text ed)))
                                                                      (string-length (editor-text ed))
                                                                      (+ (editor-cursor ed) 1)))]))


; String -> String
; produce a string like the str but remove last character
(define (string-remove-last str)
  (if (> (string-length str) 0)
      (substring str 0 (- (string-length str) 1))
      str))

(check-expect (string-remove-last "hello") "hell")
(check-expect (string-remove-last "world") "worl")
(check-expect (string-remove-last "a") "")
(check-expect (string-remove-last "") "")


(define (run text)
  (big-bang (make-editor text (string-length text))
    [to-draw render]
    [on-key edit]))

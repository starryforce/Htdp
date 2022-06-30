#lang htdp/bsl

(require 2htdp/universe)
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


; Editor KeyEvent -> Editor
; generate a new Editor according to the ke
; and current state of ed
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
                       [(= (string-length ke) 1) (cond
                                                   [(string=? ke "\b") (delete ed ke)]
                                                   [(or (string=? ke "\t") (string=? ke "\r")) ed]
                                                   [(> (+ (string-length (editor-pre ed)) (string-length (editor-post ed))) 20) ed]
                                                   [else (insert ed ke)])]
                       [(or (string=? "left" ke) (string=? "right" ke)) (move-cursor ed ke)]
                       [else ed]))

; handle \b
(check-expect (edit (make-editor "" "rv") "\b") (make-editor "" "rv"))
(check-expect (edit (make-editor "k" "rv") "\b") (make-editor "" "rv"))

; handel 1String except \b
(check-expect (edit (make-editor "kr" "v") " ") (make-editor "kr " "v"))
(check-expect (edit (make-editor "k" "v") "r") (make-editor "kr" "v"))
(check-expect (edit (make-editor "" "rv") "k") (make-editor "k" "rv"))
(check-expect (edit (make-editor "kr" "") "v") (make-editor "krv" ""))
(check-expect (edit (make-editor "" "") "v") (make-editor "v" ""))

; press \t or \r
(check-expect (edit (make-editor "k" "rv") "\t") (make-editor "k" "rv"))
(check-expect (edit (make-editor "k" "rv") "\r") (make-editor "k" "rv"))

; press left
(check-expect (edit (make-editor "kr" "v") "left") (make-editor "k" "rv"))
(check-expect (edit (make-editor "" "rv") "left") (make-editor "" "rv"))
(check-expect (edit (make-editor "kr" "") "left") (make-editor "k" "r"))
(check-expect (edit (make-editor "" "") "left") (make-editor "" ""))

; press right
(check-expect (edit (make-editor "kr" "v") "right") (make-editor "krv" ""))
(check-expect (edit (make-editor "" "rv") "right") (make-editor "r" "v"))
(check-expect (edit (make-editor "kr" "") "right") (make-editor "kr" ""))
(check-expect (edit (make-editor "" "") "right") (make-editor "" ""))

; Editor "\b" -> Editor
; delete the last letter of (editor-pre ed)
(define (delete ed ke) (make-editor
                        (string-remove-last (editor-pre ed))
                        (editor-post ed)))


; Editor KeyEvent -> Editor
; KeyEvent is 1String except "\b" \t" "\r"
; append letter ke to the end of (editor-pre ed)
(define (insert ed ke) (make-editor (string-append (editor-pre ed) ke)
                        (editor-post ed)))


; Editor KeyEvent -> Editor
; KeyEvent is one of:
; - "left"
; - "right"
; The left arrow moves the cursor one character to the left (if any),
; move the last letter of (editor-pre ed) to the start of (editor-post ed)
; and the right arrow moves it one character to the right (if any).
; move the first letter of (editor-post ed) to the end of (editor-pre ed)
; All other such KeyEvents are ignored.
(define (move-cursor ed ke) (cond
                              [(string=? ke "left") (make-editor
                                                     (string-remove-last (editor-pre ed))
                                                     (string-append (string-last (editor-pre ed)) (editor-post ed)))]
                              [(string=? ke "right") (make-editor
                                                      (string-append (editor-pre ed) (string-first (editor-post ed)))
                                                      (string-rest (editor-post ed))
                                                      )]
                              [else ed]))



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

; String -> 1String
; extracts the first character from str
(define (string-first str)
  (if (> (string-length str) 0)
      (substring str 0 1)
      str))

(check-expect (string-first "hello") "h")
(check-expect (string-first "world") "w")
(check-expect (string-first "") "")

; String -> 1String
; extracts the last character from str
(define (string-last str)
  (if (> (string-length str) 1)
      (substring str (- (string-length str) 1))
      str))

(check-expect (string-last "hello") "o")
(check-expect (string-last "world") "d")
(check-expect (string-last "") "")

; String -> String
; get the string with first character removed
(define (string-rest str)
  (if (> (string-length str) 0)
      (substring str 1)
      str))

(check-expect (string-rest "hello") "ello")
(check-expect (string-rest "world") "orld")
(check-expect (string-rest "") "")


(define (run pre)
  (big-bang (make-editor pre "")
      [to-draw render]
      [on-key edit]))

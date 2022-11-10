#lang htdp/isl+

(require 2htdp/abstraction)

; String [List-of String] -> String
; retrieves the first name on alon that is equal to, or an extension of name
(define (find-name name alon)
  (for/or ([n alon])
    (if (string-contains? name n) n #false)))


(check-expect (find-name "neo" (list "jacky" "neo" "leo")) "neo")
(check-expect (find-name "nancy" (list "jacky" "neo" "leo")) #false)
(check-expect (find-name "ne" (list "jacky" "neo" "leo")) "neo")


; Number [List-of String] -> Boolean
; ensures that no name on some list exceeds a given width 
(define (match-all? max alos)
  (local (; String -> Boolean
          ; the length of s is not exceeds max)
          (define (match? s)
            (<= (string-length s) max)))
    (andmap match? alos)))

(check-expect (match-all? 5 (list "apple" "agree" "abuse")) #true)
(check-expect (match-all? 4 (list "egg" "apple" "dog")) #false)
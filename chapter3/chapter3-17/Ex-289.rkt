#lang htdp/isl+

; String [List-of String] -> Boolean
; determines whether any of the names on the alos
; are equal to or an extension of the name.
(define (find-name name alos)
  (ormap
   ; String String -> Boolean
   ; determine if name is equal or part of name
   (lambda (s)
     (string-contains? name s)) alos))

(check-expect (find-name "neo" (list "jacky" "neo" "leo")) #true)
(check-expect (find-name "nancy" (list "jacky" "neo" "leo")) #false)
(check-expect (find-name "ne" (list "jacky" "neo" "leo")) #true)

; [List-of String] -> Boolean
; determine if all names on alos start with the letter "a".
(define (start-with-a-all? alos)
  (andmap
   ; String -> String
   ; determine if s starts with "a"
   (lambda (s)
     (string=? "a" (substring s 0 1)))alos))

(check-expect (start-with-a-all? (list "apple" "agree" "abuse")) #true)
(check-expect (start-with-a-all? (list "egg" "apple" "dog")) #false)

; Number [List-of String] -> Boolean
; ensures that no name on some list exceeds a given width 
(define (match-all? max alos)
  (andmap
   ; String -> Boolean
   ; the length of s is not exceeds max)
   (lambda (s)
     (<= (string-length s) max))alos))

(check-expect (match-all? 5 (list "apple" "agree" "abuse")) #true)
(check-expect (match-all? 4 (list "egg" "apple" "dog")) #false)
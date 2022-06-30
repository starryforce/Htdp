#lang htdp/bsl

; An Letter is one of:
; - "a" through "z"
; - #false

; A ThreeLetterWord is (make-three-letter-word Letter Letter Letter)
(define-struct three-letter-word [first second thrid])

(define ex1 (make-three-letter-word "c" "a" "t"))
(define ex2 (make-three-letter-word #false #false #false))
(define ex3 (make-three-letter-word "b" #false "z"))
(define ex4 (make-three-letter-word "b" "a" "z"))

; ThreeLetterWord ThreeLetterWord -> ThreeLetterWord
; compare origin and target, and get the result by the rule
(define (compare-word origin target)
  (make-three-letter-word
   (compare-letter (three-letter-word-first origin) (three-letter-word-first target))
   (compare-letter (three-letter-word-second origin) (three-letter-word-second target))
   (compare-letter (three-letter-word-thrid origin) (three-letter-word-thrid target))))

(check-expect (compare-word ex1 ex1) ex1)
(check-expect (compare-word ex1 ex3) ex2)
(check-expect (compare-word ex1 ex2) ex2)
(check-expect (compare-word ex3 ex4) (make-three-letter-word "b" #false "z"))

; Letter Letter -> Letter
; compare l1 and l2, if same return l1,
; if not, return #false;
(define (compare-letter l1 l2)
  (cond [(and (string? l1) (string? l2) (string=? l1 l2)) l1]
        [else #false]))

(check-expect (compare-letter "a" "a") "a")
(check-expect (compare-letter "a" "m") #false)
(check-expect (compare-letter "a" #false) #false)
(check-expect (compare-letter #false #false) #false)
(check-expect (compare-letter #false "g") #false)
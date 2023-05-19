#lang htdp/isl+

#| Exercise 399.
Louise, Jane, Laura, Dana, and Mary decide to run a lottery that assigns one gift recipient to each of them.
Since Jane is a developer, they ask her to write a program that performs this task in an impartial manner.
Of course, the program must not assign any of the sisters to herself.

Here is the core of Jane’s program:

It consumes a list of names and randomly picks one of those permutations that do not agree with the original list at any place.
Your task is to design two auxiliary functions:

Recall that random picks a random number; see exercise 99. 
|#

; [List-of String] -> [List-of String] 
; picks a random non-identity arrangement of names
(define (gift-pick names)
  (random-pick
    (non-same names (arrangements names))))
 
; [List-of String] -> [List-of [List-of String]]
; returns all possible permutations of names
; see exercise 213
(define (arrangements w)
  (local (; 1String List-of-words -> List-of-words
          ; s inserted at the beginning, between all letters, and at the end of all words of alow.
          (define (insert-everywhere/in-all-words s alow)
            (cond [(empty? alow) '()]
                  [else (append (insert-everywhere/in-one-word s 0 (first alow))
                                (insert-everywhere/in-all-words s (rest alow)))]))

          ; 1String Number Word -> List-of-words
          ; s inserted at the beginning, between all letters, and at the end of the Word w.
          (define (insert-everywhere/in-one-word s index w)
            (cond [(> index (length w)) '()]
                  [else (cons (insert-at s index 0 w)
                              (insert-everywhere/in-one-word s (add1 index) w))]))

          ; Any Number Number List -> List
          ; insert s into position index in w
          ; contrait i is less than the length of w
          (define (insert-at s i j w)
            (cond [(= i j) (cons s w)]
                  [else (cons (first w)
                              (insert-at s i (add1 j) (rest w)))])))
    (cond
      [(empty? w) (list '())]
      [else (insert-everywhere/in-all-words (first w)
                                            (arrangements (rest w)))])))

; [NEList-of X] -> X 
; returns a random item from the list 
(define (random-pick l)
  (list-ref l (random (length l))))

(check-random (random-pick '(0 1 2 3)) (random 4))
(check-random (random-pick '("am" "bob" "kim"))
              (list-ref '("am" "bob" "kim") (random 3)))

 
; [List-of String] [List-of [List-of String]] 
; -> 
; [List-of [List-of String]]
; produces the list of those lists in ll that do 
; not agree with names at any place 
(define (non-same names ll)
  (filter (lambda (i) (unique? i names)) ll))

(check-expect (non-same '("a" "b" "c") '(("a" "b" "c")
                                         ("a" "c" "b")
                                         ("b" "a" "c")
                                         ("b" "c" "a")
                                         ("c" "a" "b")
                                         ("c" "b" "a")))
              '(("b" "c" "a") ("c" "a" "b")))

; [List-of String] [List-of String] -> Boolean
; determine if alon names is different at every place
(define (unique? alon names)
  (cond [(empty? alon) #t]
        [else (and (not (string=? (first alon) (first names)))
                   (unique? (rest alon) (rest names))
                   )]))

(check-expect (unique? '() '()) #t)
(check-expect (unique? '("a") '("b")) #t)
(check-expect (unique? '("a" "b" "c") '("a" "b" "c")) #f)
(check-expect (unique? '("a" "c" "b") '("a" "b" "c")) #f)
(check-expect (unique? '("b" "a" "c") '("a" "b" "c")) #f)
(check-expect (unique? '("b" "c" "a") '("a" "b" "c")) #t)
(check-expect (unique? '("c" "a" "b") '("a" "b" "c")) #t)
(check-expect (unique? '("c" "b" "a") '("a" "b" "c")) #f)


(define ex '("Louise" "Jane" "Laura" "Dana" "Mary"))






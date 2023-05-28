#lang htdp/isl+

#| Exercise 442.
Add sort< and quick-sort< to the definitions area.
Run tests on the functions to ensure that they work on basic examples.
Also develop create-tests, a function that creates large test cases randomly.
Then explore how fast each works on various lists.

Does the experiment confirm the claim that the plain sort< function
often wins over quick-sort< for short lists and vice versa?

Determine the cross-over point.
Use it to build a clever-sort function that behaves like quick-sort<
for large lists and like sort< for lists below this cross-over point.
Compare with exercise 427.
|#

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct 
(define (quick-sort< alon)
  (local (; [List-of Number] Number -> [List-of Number]
          (define (largers alon n)
            (filter (lambda (i) (> i n)) alon))

          ; [List-of Number] Number -> [List-of Number]
          (define (smallers alon n)
            (filter (lambda (i) (< i n)) alon)))
    (cond
      [(empty? alon) '()]
      [else (local ((define pivot (first alon)))
              (append (quick-sort< (smallers alon pivot))
                      (filter (lambda (x) (= x pivot)) alon)
                      (quick-sort< (largers alon pivot))))])))
 


(check-expect (quick-sort< (list 11 8 14 7)) (list 7 8 11 14))
(check-expect (quick-sort< (list 11 8 14 8 7 8 14)) (list 7 8 8 8 11 14 14))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort< l)
  (local (; Number List-of-numbers -> List-of-numbers
          ; inserts n into the sorted list of numbers l 
          (define (insert n l)
            (cond
              [(empty? l) (cons n '())]
              [else (if (<= n (first l))
                        (cons n l)
                        (cons (first l) (insert n (rest l))))])))
    (cond
      [(empty? l) '()]
      [(cons? l) (insert (first l) (sort< (rest l)))])))
 
(check-expect (sort< (list 11 8 14 7)) (list 7 8 11 14))
(check-expect (sort< (list 11 8 14 8 7 8 14)) (list 7 8 8 8 11 14 14))

; N -> [List-of Number]
; generate a list contains n numbers;
(define (create-tests n)
  (build-list n (lambda (i) (random n))))

(check-expect (length (create-tests 9)) 9)
(check-expect (length (create-tests 200)) 200)

; Function N -> N
(define (perf fn n)
  (local ((define dataset (build-list 2000 (lambda (i) (create-tests n))))
          (define result (map fn dataset)))
    n))

(define LENGTH 85)

(time (perf quick-sort< LENGTH))
(time (perf sort< LENGTH))

#|
cpu time: 1796 real time: 1826 gc time: 109
20
cpu time: 546 real time: 554 gc time: 31
20

cpu time: 515 real time: 519 gc time: 15
60
cpu time: 359 real time: 367 gc time: 0
60

cpu time: 718 real time: 704 gc time: 62
80
cpu time: 687 real time: 677 gc time: 15
80

cpu time: 953 real time: 1032 gc time: 46
90
cpu time: 968 real time: 979 gc time: 31
90

cpu time: 8734 real time: 8860 gc time: 421
100
cpu time: 10500 real time: 10611 gc time: 828
100
|#

; cross-over point is 85

; [List-of Number] -> [List-of Number]
; produces the sorting version of l
; as fast as possible
(define (clever-sort l)
  (local ((define counts (length l)))
    (if (< counts 85)
        (sort< l)
        (quick-sort< l))))

(check-expect (clever-sort (list 11 8 14 7)) (list 7 8 11 14))
(check-expect (clever-sort (list 11 8 14 8 7 8 14)) (list 7 8 8 8 11 14 14))
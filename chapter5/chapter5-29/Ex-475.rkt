#lang htdp/isl+

#| Exercise 475.
Redesign find-path/list so that it uses an existing list abstraction
from figures 95 and 96 instead of explicit structural recursion.
Hint Read the documentation for Racket’s ormap.
How does it differ from ISL+’s ormap function?
Would the former be helpful here?
|#

; A Node is a Symbol.

; A Graph is:
; [List-of [List-of Node]]

(define sample-graph
  (list (list 'A 'B 'E)
        (list 'B 'E 'F)
        (list 'C 'D)
        (list 'D)
        (list 'E 'C 'F)
        (list 'F 'D 'G)
        (list 'G)))

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false

; find-path will go for the next letter first.
(check-expect (find-path 'A 'G sample-graph) '(A B E F G))

(define (find-path origination destination G)
  (local (; Node Graph -> [List-of Node]
          ; produces the list of immediate neighbors of n in g.
          (define (neighbors n g)
            (cond [(empty? g) (error "not found")]
                  [else (if (symbol=? n (first (first g)))
                            (rest (first g))
                            (neighbors n (rest g)))]))
          ; [List-of Node] Node Graph -> [Maybe Path]
          ; finds a path from some node on lo-Os to D
          ; if there is no path, the function produces #false
          (define (find-path/list lo-Os D G)
            (foldl (lambda (cur prev)
                     (local ((define candidate
                               (find-path cur D G)))
                       (cond
                         [(boolean? prev) candidate]
                         [else prev]))) #false lo-Os)))
    (cond
      [(symbol=? origination destination) (list destination)]
      [else (local ((define next (neighbors origination G))
                    (define candidate
                      (find-path/list next destination G)))
              (cond
                [(boolean? candidate) #false]
                [else (cons origination candidate)]))])))
 
; Racket's ormap will return the first result that is not #f,
; ISL+'s ormap will return either #t or #f.
; it will get the result simpler than ISL+'s ormap
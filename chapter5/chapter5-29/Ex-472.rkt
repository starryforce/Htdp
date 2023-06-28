#lang htdp/isl+

#| Exercise 472.
Develop a test for find-path on the inputs 'A, 'G, and sample-graph.
Take a look at figure 168. Which path will the function find? Why?
Design test-on-all-nodes, a function that consumes a graph g and
determines whether there is a path between every pair of nodes.
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

; Node Graph -> [List-of Node]
; produces the list of immediate neighbors of n in g.
(check-expect (neighbors 'E sample-graph) '(C F))
(check-expect (neighbors 'D sample-graph) '())
(define (neighbors n g)
  (cond [(empty? g) (error "not found")]
        [else (if (symbol=? n (first (first g)))
                  (rest (first g))
                  (neighbors n (rest g)))]))

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false

; find-path will go for the next letter first.
(check-expect (find-path 'A 'G sample-graph) '(A B E F G))

(define (find-path origination destination G)
  (cond
    [(symbol=? origination destination) (list destination)]
    [else (local ((define next (neighbors origination G))
                  (define candidate
                    (find-path/list next destination G)))
            (cond
              [(boolean? candidate) #false]
              [else (cons origination candidate)]))]))
 
; [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-Os to D
; if there is no path, the function produces #false
(define (find-path/list lo-Os D G)
  (cond
    [(empty? lo-Os) #false]
    [else (local ((define candidate
                    (find-path (first lo-Os) D G)))
            (cond
              [(boolean? candidate)
               (find-path/list (rest lo-Os) D G)]
              [else candidate]))]))
; Graph -> Boolean
; determines whether there is a path between every pair of nodes
(check-expect (test-on-all-nodes (list (list 'A 'B) (list 'B))) #false)
(check-expect (test-on-all-nodes (list (list 'A 'B) (list 'B 'A))) #true)
;(check-expect (test-on-all-nodes (list (list 'A 'B 'C) (list 'B 'A 'C) (list 'C 'A 'B))) #true)
;(check-expect (test-on-all-nodes (list (list 'A 'B 'C) (list 'B 'A) (list 'C 'A 'B))) #true)
;(check-expect (test-on-all-nodes (list (list 'A 'B) (list 'B 'C) (list 'C 'A))) #true)
(define (test-on-all-nodes g)
  (local ((define nodes (map (lambda (x) (first x)) g)))
    (andmap (lambda (x) (cons? (find-path (first x) (second x) g))) (get-all-pairs nodes))))

; [List-of Any] -> [List-of (list Any Any)]
; get all pairs constructed by items in l.
; constrait every item in l is unique.
(check-expect (get-all-pairs '(A B)) '((A B) (B A)))
(check-expect (get-all-pairs '(A B C)) '((A B) (A C) (B A) (B C) (C A) (C B)))
(define (get-all-pairs l)
  (foldl (lambda (current base)
           (local (; every item expect current item
                   (define others (filter (lambda (y) (not (symbol=? current y))) l))
                   ; every pairs starts with current
                   (define pairs (map (lambda (z) (list current z)) others)))
             (append base pairs)))
         '()
         l))
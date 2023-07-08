#lang htdp/isl+

#| Exercise 492.
Modify the definitions in figure 169 so that the program produces #false,
even if it encounters the same origin twice.
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
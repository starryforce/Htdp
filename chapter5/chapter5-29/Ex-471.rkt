#lang htdp/isl+

#| Exercise 471.
Translate one of the above definitions into proper list form using list and proper symbols.
The data representation for nodes is straightforward:
Formulate a data definition to describe the class of all Graph representations,
allowing an arbitrary number of nodes and edges.
Only one of the above representations has to belong to Graph.
Design the function neighbors.
It consumes a Node n and a Graph g and produces the list of immediate neighbors of n in g.
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


#lang htdp/bsl+

(require 2htdp/web-io)

(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

; List-of-strings -> List-of-songs
(define (ranking los)
  (reverse (add-ranks (reverse los))))

; List-of-strings -> List-of-ranks
; use a list to represent a song with it's ranking
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))

; A Rank is a (list number string)
; A List-of-ranks is one of:
; - '()
; - (cons Rank List-of-ranks)

; List-of-ranks -> ... nested list ...
; creates an HTML table from list of ranks
(define (make-ranking lor)
  `(html
    (body
     (table ((border "1"))
            ,@(make-rows lor)))))

; List-of-ranks -> ... nested list ...
; creates rows for an HTML table from rows lor
(define (make-rows lor)
  (cond [(empty? lor) '()]
        [else (cons `(tr ,@(make-row (first lor)))
                   (make-rows (rest lor)))]))


; Rank -> ... nested list ...
; creates a row for an HTML table from l
(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l))
                (make-row (rest l)))]))
 
; Number | String -> ... nested list ...
; creates a cell for an HTML table from a number 
(define (make-cell n)
  `(td ((align "center")) ,(if (string? n) n (number->string n))))


(define html0 (make-ranking (ranking one-list)))
(show-in-browser html0)

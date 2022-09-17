#lang htdp/bsl+

(require 2htdp/image)

(define WIDTH 10) ; # of blocks, horizontally
(define HEIGHT (* 2 WIDTH)) ; # of blocks, horizontally
(define SIZE 10) ; blocks are squares
(define SCENE-SIZE (* WIDTH SIZE))
(define SCENE (empty-scene SCENE-SIZE (* HEIGHT SIZE)))
 
 
(define BLOCK ; red squares with black rims
  (overlay
    (square (- SIZE 1) "solid" "red")
    (square SIZE "outline" "black")))

(define-struct tetris [block landscape])
(define-struct block [x y])
 
; A Tetris is a structure:
;   (make-tetris Block Landscape)
; A Landscape is one of: 
; – '() 
; – (cons Block Landscape)
; A Block is a structure:
;   (make-block N N)
 
; interpretations
; (make-block x y) depicts a block whose left 
; corner is (* x SIZE) pixels from the left and
; (* y SIZE) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and ... are resting


(define block-dropping (make-block 5 20))
(define block-landed (make-block 0 (- HEIGHT 1)))
(define block-on-block (make-block 0 (- HEIGHT 2)))
(define landscape0 (list block-landed block-on-block))
(define tetris0 (make-tetris block-dropping '()))
(define tetris0-drop (make-tetris block-dropping landscape0))

; Tetris -> Image
; put Block onto the scene
(define (render tt)
  (render-blocks (cons (tetris-block tt)
                       (tetris-landscape tt))))

(check-expect (render tetris0)
              (place-image/align BLOCK
                                 (* 5 SIZE)
                                 (* 20 SIZE)
                                 "left"
                                 "top"
                                 SCENE))
(check-expect (render tetris0-drop)
              (place-image/align BLOCK
                                 (* 5 SIZE)
                                 (* 20 SIZE)
                                 "left"
                                 "top"
                                 (place-image/align BLOCK
                                                    0
                                                    (* (- HEIGHT 1) SIZE)
                                                    "left"
                                                    "top"
                                                    (place-image/align BLOCK
                                                                       0
                                                                       (* (- HEIGHT 2) SIZE)
                                                                       "left"
                                                                       "top"
                                                                       SCENE))))
; List-of-blocks -> Image
; put Blocks onto the scene
(define (render-blocks alob)
  (cond [(empty? alob) SCENE]
        [else (place-image/align BLOCK
                                 (* SIZE (block-x (first alob)))
                                 (* SIZE (block-y (first alob)))
                                 "left"
                                 "top"
                                 (render-blocks (rest alob)))]))
(check-expect (render-blocks '()) SCENE)
(check-expect (render-blocks (cons block-dropping landscape0))
              (place-image/align BLOCK
                                 (* 5 SIZE)
                                 (* 20 SIZE)
                                 "left"
                                 "top"
                                 (place-image/align BLOCK
                                                    0
                                                    (* (- HEIGHT 1) SIZE)
                                                    "left"
                                                    "top"
                                                    (place-image/align BLOCK
                                                                       0
                                                                       (* (- HEIGHT 2) SIZE)
                                                                       "left"
                                                                       "top"
                                                                       SCENE))))


                           


                           
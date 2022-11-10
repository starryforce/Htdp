#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 12) ; # of blocks, horizontally
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
(define tetris1-drop (make-tetris block-landed '()))
(define tetris2-drop (make-tetris block-on-block (list block-landed)))

; Tetris -> Image
; put Block onto the scene
(define (render tt)
  (render-blocks (cons (tetris-block tt)
                       (tetris-landscape tt))))

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
; Tetris -> Tetris
; the dropping block fall down 1 unit every tock
; when the block reach the landscape, the block turn rest
; at the top generate another new block
(define (tock tt)
  (cond [(landed? tt) (make-tetris (generate-block (tetris-block tt))  (cons (tetris-block tt) (tetris-landscape tt)) )]
        [else (make-tetris (fall (tetris-block tt)) (tetris-landscape tt))]))

; Block -> Block
; generate a new block at top, one unit right of b
(define (generate-block b)
  (make-block (modulo (add1 (block-x b)) WIDTH) 0))

(check-expect (generate-block (make-block 5 20)) (make-block 6 0))
(check-expect (generate-block (make-block (sub1 WIDTH) 18)) (make-block 0 0))
(check-expect (generate-block (make-block 0 18)) (make-block 1 0))

;  Tetris -> Boolean
; determine if tt land on the landscape
; - land on the bottom
; - land on other block
(define (landed? tt)
        ; land on the bottom
  (cond [(= (block-y (tetris-block tt)) (sub1 HEIGHT)) #true]
        ; land on the landscape
        [(land-on-block? (tetris-block tt) (tetris-landscape tt)) #true]
        [else #false]))

(check-expect (landed? tetris0-drop) #false)
(check-expect (landed? tetris1-drop) #true)
(check-expect (landed? tetris2-drop) #true)

; Block Landscape -> Boolean
; determine if b land on l
(define (land-on-block? b l)
  (cond [(empty? l) #false]
        [else (or (above-block? b (first l))
                  (land-on-block? b (rest l)))]))

(check-expect (land-on-block? block-dropping landscape0) #false)
(check-expect (land-on-block? block-landed '()) #false)
(check-expect (land-on-block? block-on-block (list block-landed)) #true)

; Block -> Block
; determine if b1 is above b2
(define (above-block? b1 b2)
  (and (= (block-x b1) (block-x b2))
       (= (add1 (block-y b1)) (block-y b2))))

(check-expect (above-block? block-dropping block-landed) #false)
(check-expect (above-block? block-on-block block-landed) #true)

; Block -> Block
; the block b fall down one unit
(define (fall b)
  (make-block (block-x b) (add1 (block-y b))))

(check-expect (fall block-dropping) (make-block 5 21))


(check-expect (tock tetris0-drop)
              (make-tetris (make-block 5 21) landscape0))
(check-expect (tock tetris1-drop)
              (make-tetris (make-block 1 0) (list block-landed)))
(check-expect (tock tetris2-drop)
              (make-tetris (make-block 1 0) (list block-on-block block-landed)))

; Tetris KeyEvent -> Tetris
; use arrow key "left" or "right" to control dropping block
(define (control tt ke)
  (make-tetris (cond [(or (string=? ke "left") (string=? ke "right"))
                      (if (member? (control-block (tetris-block tt) ke) (tetris-landscape tt))
                          (tetris-block tt)
                          (control-block (tetris-block tt) ke))]
                     [else (tetris-block tt)])
               (tetris-landscape tt)))

(check-expect (control (make-tetris (make-block 0 20) '()) "left") (make-tetris (make-block 0 20) '()))
(check-expect (control (make-tetris (make-block (sub1 WIDTH) 20) '()) "right")
              (make-tetris (make-block (sub1 WIDTH) 20) '()))
(check-expect (control tetris0 "right") (make-tetris (make-block 6 20) '()))
(check-expect (control tetris0 "left") (make-tetris (make-block 4 20) '()))
(check-expect (control tetris0 "a") tetris0)
(check-expect (control (make-tetris (make-block 0 (- HEIGHT 2)) (list (make-block 1 (- HEIGHT 2)) (make-block 1 (- HEIGHT 1))))  "right")
              (make-tetris (make-block 0 (- HEIGHT 2)) (list (make-block 1 (- HEIGHT 2)) (make-block 1 (- HEIGHT 1)))))
(check-expect (control (make-tetris (make-block 2 (- HEIGHT 2)) (list (make-block 1 (- HEIGHT 2)) (make-block 1 (- HEIGHT 1))))  "left")
              (make-tetris (make-block 2 (- HEIGHT 2)) (list (make-block 1 (- HEIGHT 2)) (make-block 1 (- HEIGHT 1)))))
              

; Block Direction -> Block
; Directions is one of:
; - "left"
; - "right"
; 
(define (control-block b direction)
  (cond [(string=? direction "left") (make-block (max (sub1 (block-x b)) 0) (block-y b))]
        [(string=? direction "right") (make-block (min (add1 (block-x b)) (sub1 WIDTH)) (block-y b))]))

(check-expect (control-block (make-block 0 10) "left") (make-block 0 10))
(check-expect (control-block (make-block 10 10) "left") (make-block 9 10))
(check-expect (control-block (make-block 10 10) "right") (make-block 11 10))
(check-expect (control-block (make-block (sub1 WIDTH) 10) "right") (make-block (sub1 WIDTH) 10))

; Tetris -> Boolean
; determine if any column reach the top of cancas
(define (game-end tt)
  (some-at-top? (tetris-landscape tt)))

; Landscape -> Boolean
; some of the block in alol 's y-coordinate is 0
(define (some-at-top? alol)
  (cond [(empty? alol) #false]
        [else (or (= (block-y (first alol)) 0)
                  (some-at-top? (rest alol))
                   )]))

; Tetris -> Image
; display the score when game end
(define (game-end-render tt) (place-image/align (text (string-append "the game end, your score is "
                                                                     (number->string (length (tetris-landscape tt))))
                                                      16
                                                      "green")
                                                0
                                                (image-height SCENE)
                                                "left"
                                                "bottom"
                                                (render tt)))

(check-expect (some-at-top? (list (make-block 5 0))) #true)
(check-expect (some-at-top? (list (make-block 5 10))) #false)

(define (tetris-main rate)
  (big-bang tetris0
    [on-draw render]
    [on-key control]
    [on-tick tock rate]
    [stop-when game-end game-end-render]))

(tetris-main 0.02)
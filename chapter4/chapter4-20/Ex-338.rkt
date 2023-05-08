#lang htdp/isl+

(require htdp/dir)

(define W (create-dir "D:\\Work"))

; File* -> Number
; count the file's quantity
(define (how-many-files fl)
  (cond [(empty? fl) 0]
        [else (+ 1 (how-many-files (rest fl)))]))

; Dir* -> Number
; determines how many files in dl
(define (how-many-dirs dl)
  (cond [(empty? dl) 0]
        [else (+ (how-many (first dl))
                 (how-many-dirs (rest dl)))]))

; Dir.v3 -> Number
; determines how many files d contains
(define (how-many d)
  (+ (how-many-dirs (dir-dirs d))
     (how-many-files (dir-files d))))

(how-many W)
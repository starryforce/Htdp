;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ex-17) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define (image-classify img)
  (cond
    [(> (image-height img) (image-width img)) "tall"]
    [(< (image-height img) (image-width img)) "wide"]
    [else "square"]
    )
  )

(image-classify (rectangle 60 40 "solid" "red"))
(image-classify (rectangle 40 60 "solid" "red"))
(image-classify (rectangle 60 60 "solid" "red"))
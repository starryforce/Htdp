#lang htdp/isl+

(require 2htdp/batch-io)

#| Exercise 510.
Many operating systems come with the fmt program,
which can rearrange the words in a file so that
all lines in the resulting file have a maximal width.
As a widely used program, fmt supports a range of related functions.
This exercise focuses on its core functionality.

Design the program fmt. It consumes a natural number w,
the name of an input file in-f, and the name of an output file out-f—in
the same sense as read-file from the 2htdp/batch-io teachpack.
Its purpose is to read all the words from the in-f,
to arrange these words in the given order into lines of maximal width w,
and to write these lines to out-f. |#

; (read-words "Ex-510-data.txt")

; N String String -> String
; read content from in-f, recombine the words in in-f
; into lines that have max length w.
; finally write the result into out-f
(define (fmt w in-f out-f)
  (write-file out-f (foldl (lambda (cur prev) (string-append prev cur "\n"))
                           ""
                           (map (lambda (l) (foldl (lambda (cur prev) (string-append prev cur " ")) "" l))
                                (fmt-core (read-words in-f) w)))))

(define ex (list "hello" "world" "good" "bye" "i," "for" "1," "am" "done"))
; [List-of String] -> [List-of [List-of String]]
; rearrange the alos into a list
; which the total symbol count on every item in it  is less than w
(check-expect (fmt-core ex 11) (list (list "hello" "world") (list "good" "bye" "i,") (list "for" "1," "am") (list "done")))
(check-expect (fmt-core ex 16) (list (list "hello" "world" "good") (list "bye" "i," "for" "1," "am") (list "done")))
(define (fmt-core alos0 w)
  (local (; [List-of String] [List-of [List-of String]] -> [List-of [List-of String]]
          ; accumulator a is a target list from items which alos lacks from alos0
          ; unit is a temp list stash the words to be added into a
          (define (fmt-core/a alos unit a)
            (cond [(empty? alos) (if (empty? unit) a (append a (list unit)))]
                  [else (local ((define tmp (append unit (list (first alos)))))
                          (cond [(<= (+ (sub1 (length tmp))
                                        (foldl (lambda (cur prev) (+ (string-length cur) prev)) 0 tmp))
                                     w)
                                 (fmt-core/a (rest alos) (append unit (list (first alos))) a)]
                                [else
                                 (fmt-core/a alos '() (append a (list unit)))]))])))
    (fmt-core/a alos0 '() '())))

(fmt 20 "Ex-510-data.txt" "output.bak")
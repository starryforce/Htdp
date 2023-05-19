#lang htdp/isl+

(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

#| Exercise 396.
Hangman is a well-known guessing game. WIKI: https://en.wikipedia.org/wiki/Hangman_(game)
One player picks a word, the other player gets told how many letters the word contains.
The latter picks a letter and asks the first player whether and where this letter occurs in the chosen word.
The game is over after an agreed-upon time or number of rounds.

Figure 136 presents the essence of a time-limited version of the game.
See Local Definitions Add Expressive Power for why checked-compare is defined locally.

The goal of this exercise is to design compare-word, the central function.
It consumes the word to be guessed,
a word s that represents how much/little the guessing player has discovered,
and the current guess.
The function produces s with all "_" where the guess revealed a letter.

Once you have designed the function, run the program like this:
(define LOCATION "/usr/share/dict/words") ; on OS X
(define AS-LIST (read-lines LOCATION))
(define SIZE (length AS-LIST))
(play (list-ref AS-LIST (random SIZE)) 10)
See figure 74 for an explanation. Enjoy and refine as desired! 
|#

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))


; An HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed 
 
; HM-Word N -> String
; runs a simplistic hangman game, produces the current state
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
          ; HM-Word KeyEvent -> HM-Word 
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key  checked-compare]))))
 
; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))

; HM-Word HM-Word Letter -> HM-Word
; produces current with all "_" where the guess revealed guess.
(define (compare-word target current guess)
  (cond [(empty? target) '()]
        [else (cons (if (string=? guess (first target))
                        (first target)
                        (first current))
                    (compare-word (rest target) (rest current) guess))]))

(check-expect (compare-word '() '() "a") '())
(check-expect (compare-word '("a") '("_") "b") '("_"))
(check-expect (compare-word '("a") '("_") "a") '("a"))
(check-expect (compare-word (explode "apple") (explode "____e") "a") (explode "a___e"))
(check-expect (compare-word (explode "apple") (explode "____e") "q") (explode "____e"))
(check-expect (compare-word (explode "apple") (explode "_____") "p") (explode "_pp__"))
(check-expect (compare-word (explode "apple") (explode "a____") "a") (explode "a____"))


;(define LOCATION "/usr/share/dict/words") ; on OS X
(define LOCATION "C:/Users/StarryForce/OneDrive/words")
(define AS-LIST (read-lines LOCATION))
(define SIZE (length AS-LIST))
(play (list-ref AS-LIST (random SIZE)) 10)
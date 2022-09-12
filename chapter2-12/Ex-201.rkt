#lang htdp/bsl+

(require 2htdp/batch-io)
(require 2htdp/itunes)

(define date1 (create-date 2012 1 13 14 56 19))
(define date2 (create-date 2002 9 8 17 4 38))
(define date3 (create-date 2011 2 10 14 18 36))
(define date4 (create-date 2012 3 16 13 32 11))
(define date5 (create-date 2002 8 3 22 30 00))
(define date6 (create-date 2013 5 7 22 21 49))

(define track1 (create-track "Monday Morning" "Fleetwood Mac" "Fleetwood Mac" 167826 1 date3 5 date4))
(define track2 (create-track "Distino Di Bilita" "Cesaria Evora" "Distino Di Belita" 304169 8 date1 2 date2))
(define track3 (create-track "China Roses" "Enya" "The Memory Of Trees" 289854 6 date5 9 date6))

(define ltracks0 '())
(define ltracks1 (list track1 track3))
(define ltracks2 (list track1 track2 track3))


; LTracks -> List-of-strings
; produces the list of album titles in alot
(define (select-all-album-titles alot)
  (cond [(empty? alot) '()]
        [else (cons (track-album (first alot))
                    (select-all-album-titles (rest alot)))]))

(check-expect (select-all-album-titles ltracks0) '())
(check-expect (select-all-album-titles ltracks1) (list "Fleetwood Mac"  "The Memory Of Trees"))
(check-expect (select-all-album-titles ltracks2) (list "Fleetwood Mac" "Distino Di Belita" "The Memory Of Trees"))


; List-of-strings -> List-of-strings
; construct a list of strings which every String from alos exactly once
(define (create-set alos)
  (cond [(empty? alos) ...]
        [else (.. (first alos) ....
                  (create-set (rest alos)) ...)]))

(check-expect (create-set '()) '())
(check-expect (create-set "a" "a" "b" "c") (list "a" "b" "c"))
(check-expect (create-set "a" "b" "c") (list "a" "b" "c"))

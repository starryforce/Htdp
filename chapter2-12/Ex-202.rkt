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

; String LTracks -> LTracks
; extracts from alot the list of tracks that belong to the given album
(define (select-album a alot)
  (cond [(empty? alot) '()]
        [else (if (match a (first alot))
                  (cons (first alot) (select-album a (rest alot)))
                  (select-album a (rest alot)))]))

(check-expect (select-album "Fleetwood Mac" ltracks0) '())
(check-expect (select-album "Fleetwood Mac" ltracks2) (list track1))
(check-expect (select-album "Distino Di Belita" ltracks1) '())


; String Track -> Boolean
; determine if t belongs to the album a
(define (match a t)
  (string=? a (track-album t)))


(check-expect (match "Fleetwood Mac" track1) #true)
(check-expect (match "Fleetwood Mac" track2) #false)
(check-expect (match "abc" track3) #false)
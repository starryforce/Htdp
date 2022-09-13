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


; A LLTracks is one of:
; - '()
; - (cons LTracks LLTracks)


; LTracks -> LLTracks
; produces a list of LTracks, one per album.
; Each album is uniquely identified by its title and shows up in the result only once
(define (select-albums alot)
  (generate-list (select-album-titles/unique alot) alot))

(check-expect (select-albums ltracks0) '())
(check-expect (select-albums ltracks1) (list (list track1) (list track3)))
(check-expect (select-albums ltracks2) (list (list track1) (list track2) (list track3)))

; List-of-strings -> LTracks
; produces a list of LTracks, one per item in alos.
(define (generate-list alos alot)
  (cond [(empty? alos) '()]
        [else (cons (select-album (first alos) alot)
                    (generate-list (rest alos) alot))]))

(check-expect (generate-list (list "Fleetwood Mac") ltracks2) (list (list track1)))


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
  (cond [(empty? alos) '()]
        [else (cons (first alos)
                    (create-set (remove-s (first alos) (rest alos))))]))

(check-expect (create-set '()) '())
(check-expect (create-set (list "a" "a" "b" "c")) (list "a" "b" "c"))
(check-expect (create-set (list "a" "b" "c")) (list "a" "b" "c"))

; String List-of-strings -> List-of-strings
; remove every string equal to s in alos
(define (remove-s s alos)
  (cond [(empty? alos) '()]
        [else (if (string=? s (first alos))
                  (remove-s s (rest alos))
                  (cons (first alos) (remove-s s (rest alos))))]))


(check-expect (remove-s "a" (list "a" "a" "b" "c")) (list "b" "c"))
(check-expect (remove-s "a" (list "a")) '())
(check-expect (remove-s "a" '()) '())

; LTracks -> List-of-strings
; produces a list of unique album titles i alot
(define (select-album-titles/unique alot)
  (create-set
   (select-all-album-titles alot)))

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
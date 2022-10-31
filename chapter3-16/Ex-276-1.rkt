#lang htdp/isl

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

; String Date LTracks -> LTracks
; filter all tracks that belong to album and
; have been played after date
(define (select-album-date album date alot)
  (local (; Track -> Boolean
          ; determine if the t match the requirement
          (define (fn t) (match? album date t))
          )
    (filter fn alot)))

(check-expect (select-album-date "Distino Di Belita" date5 '()) '())
(check-expect (select-album-date "Distino Di Belita" date5 ltracks2) (list track2))
(check-expect (select-album-date "ABC" date5 ltracks2) '())
(check-expect (select-album-date "Distino Di Belita" date6 ltracks2) '())

; String Date Track -> Boolean
; determine if the track meet the requirement
; belongs to the album and played after date
(define (match? album date track)
  (and (string=? (track-album track) album)
       (date-before? date (track-played track))))

(check-expect (match? "Distino Di Belita" date5 track2) #true)
(check-expect (match? "ABC" date5 track2) #false)
(check-expect (match? "Distino Di Belita" date6 track2) #false)


; Date -> Date
; determine if date d1 is before d2
(define (date-before? d1 d2)
  (cond [(< (date-year d1) (date-year d2)) #true]
        [(= (date-year d1) (date-year d2))
         (cond [(< (date-month d1) (date-month d2)) #true]
               [(= (date-month d1) (date-month d2))
                (< (daytime->seconds d1) (daytime->seconds d2))]
               [else #false])]
        [else #false]))

(check-expect (date-before? date1 date2) #false)
(check-expect (date-before? date1 date4) #true)

; Date -> Number
; converts day hour minute second to second
(define (daytime->seconds d)
  (+ (* (date-day d) 24 60 60) (* (date-hour d) 60 60 ) (* (date-minute d) 60) (date-second d)))

(check-expect (daytime->seconds (create-date 1990 3 0 10 14 3)) 36843)
(check-expect (daytime->seconds (create-date 1933 1 0 4 3 53)) 14633)
(check-expect (daytime->seconds (create-date 2022 4 1 0 4 13)) 86653)



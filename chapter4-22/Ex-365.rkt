#lang htdp/isl+

#| Exercise 365.
Interpret the following elements of Xexpr.v2 as XML data:

'(server ((name "example.org")))

'(carcas (board (grass)) (player ((name "sam"))))

'(start)

Which ones are elements of Xexpr.v0 or Xexpr.v1?

|#

; <server name="example.org" />

; <carcas>
;   <board>
;     <grass />
;   </board>
;   <player name="sam" />
; </carcas>

; <start />

; the third one is element of Xexpr.v0

#lang htdp/bsl

; because "\t" "\r" meet the (= (string-length k) 1) as well
; and they should be handle as a special case first.

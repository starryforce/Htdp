#lang htdp/isl+

#| Exercise 438.
In your words: how does greatest-divisor-<= work?
Use the design recipe to find the right words.
Why does the locally defined greatest-divisor-<= recur on (min n m)?
|#

; from large to small, check every possible number until find the correct answer.

; in this question, N is one of
; - 1
; - (add1 N)
; we need to find the evenly divisor of the two input,
; so n m could be recursive at the same time.
; the largest possible evenly divisor is the smaller in n m
; so the recursion starts with (min n m), end with 1.
; so when we find a number match our require, it should be the
; largest because all numbers larger than it didn't match.
#lang htdp/isl+

#| Exercise 495.
Complete the manual evaluation of (sum/a '(10 4) 0) in figure 183.
Doing so shows that the sum and sum.v2 add up the given numbers in reverse order.
While sum adds up the numbers from right to left, the accumulator-style version adds them up from left to right.

Note on Numbers Remember that for exact numbers,
this difference has no effect on the final result.
For inexact numbers, the difference can be significant.
See the exercises at the end of Intermezzo 5: The Cost of Computation. |#

#|
(sum.v2 '(10 4))
== (sum/a '(10 4) 0)
== (sum/a '(4) (+ 10 0))
== (sum/a '(4) 10)
== (sum/a '() (+ 4 10))
== (sum/a '() 14)
== 14
|#
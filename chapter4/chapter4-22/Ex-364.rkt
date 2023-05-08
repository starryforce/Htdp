#lang htdp/isl+

#| Exercise 364.
Represent this XML data as elements of Xexpr.v2:

<transition from="seen-e" to="seen-f" />

<ul><li><word /><word /></li><li><word /></li></ul>

Which one could be represented in Xexpr.v0 or Xexpr.v1? 
|#

'(transition ((from "seen-e") (to "seen-f")))

'(ul
  (li
   (word)
   (word))
  (li
   (word)))

; the second could be represetend i Xexpr.v1
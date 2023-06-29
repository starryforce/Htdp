#lang htdp/isl+

#| Exercise 478.
You can also place the first queen in all squares of the top-most row,
the right-most column, and the bottom-most row.
Explain why all of these solutions are just like the three scenarios depicted in figure 173.

This leaves the central square.
Is it possible to place even a second queen
after you place one on the central square of a 3 by 3 board?
|#

; it's just rotate 90 180 270 angle to get latter situations

; if the first place in central square, all the remaining square are threatended.
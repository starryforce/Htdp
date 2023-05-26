#lang htdp/isl+

#| Exercise 431.
Answer the four key questions for the bundle problem and
the first three questions for the quick-sort< problem.
How many instances of generate-problem are needed?
|#

#| bundle
1. when the input is empty list
2. '()
3. divide into two problem, pick the first n items, drop the first n items.
4. combine picked list with genetive recursion's result
need 2 generate-problem
|#

#| quick-sort<
1. when the list to be sort is empty
2. '()
3. divide into three problems,
  a. sort all numbers larger than pivot
  b. sort all numbers smaller than pivot
  c. combine pivot with these two list
need 3 generate-problem
|#
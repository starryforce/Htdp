#lang htdp/isl+

#|
Q: How many times does a file name read! occur in the directory tree TS?
A: 2

Q: Can you describe the path from the root directory to the occurrences?
A: 1. TS - read! 2. Ts - Libs - Docs - read!

Q: What is the total size of all the files in the tree?
A: 207 = (+ 99 52 17 10 8 2 19) 

Q: What is the total size of the directory if each directory node has size 1?
A: 212 = (+ 207 1 1 1 1 1)

Q: How many levels of directories does it contain?
A: 4

|#
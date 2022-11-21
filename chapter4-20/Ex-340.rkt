#lang htdp/isl+

(require htdp/dir)

(define EX (create-dir "C:\\Users\\StarryForce\\OneDrive\\algorithm"))


; Dir -> [List-of String]
; lists the names of all files and directories in d
(define (ls d)
  (cons (dir-name d)
        (append (ls-dirs (dir-dirs d))
                (ls-files (dir-files d)))))

(check-expect (length (ls EX)) 7)


; [List-of Dir] -> [List-of String]
; ; lists the names of all files and directories in dirs
(define (ls-dirs dirs)
  (cond [(empty? dirs) '()]
        [else (append (ls (first dirs))
                      (ls-dirs (rest dirs)))]))

(check-expect (length (ls-dirs (dir-dirs EX))) 2)


; [List-of Files] -> [List-of String]
; ; lists the names of all files and directories in files
(define (ls-files files)
  (cond [(empty? files) '()]
        [else (cons (file-name (first files))
                    (ls-files (rest files)))]))

(check-expect (length (ls-files (dir-files EX))) 4)
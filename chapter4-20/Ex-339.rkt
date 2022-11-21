#lang htdp/isl+

(require htdp/dir)

(define EX (create-dir "D:\\Work"))


; 1. 写三件套
; 2. 写测试用例
; 3. 写模板，入参结构类型，所以列出所有选择器
; 4. 因为其中含有两种数据类型，所以为二者增加各自独立的【三件套、测试】
; 7. (dir-dirs d) (dir-files d) 分别调用对应的辅助函数
; Dir String -> Boolean
; determines whether or not a file with name n occurs in d
(define (find? d n)
  (or (handle-dirs (dir-dirs d) n)
      (handle-files (dir-files d) n)))

(check-expect (find? EX "PC前端.xlsx") #true)
(check-expect (find? EX "World.db3") #true)
(check-expect (find? EX "abc.efg") #false)

; 5. 写模板，入参是 List
; 5-1. 列表项是 Dir，加辅助函数调用
; 5-2. (rest dir) 仍是 [List-of Dir],加自然递归
; [List-of Dir] String -> Boolean
; determines whether a file with name n occurs in dirs
(define (handle-dirs dirs n)
  (cond [(empty? dirs) #false]
        [else (or (find? (first dirs) n)
                  (handle-dirs (rest dirs) n))]))

(check-expect (handle-dirs (dir-dirs EX) "PC前端.xlsx") #f)
(check-expect (handle-dirs (dir-dirs EX) "World.db3") #t)
(check-expect (handle-dirs (dir-dirs EX) "abc.efg") #f)

; 6. 写模板,入参是 List
; 6-1. 列表项是 File
; 6-2. (rest dir) 仍是 [List-of Dir],加自然递归
; 6-3. (first files) 是 File 类型，加一个辅助函数判断文件是否是特定文件名
; [List-of Dir] String -> Boolean
; determines whether a file with name n equals to one of files
(define (handle-files files n)
  (cond [(empty? files) #f]
        [else (or (handle-file (first files) n)
                  (handle-files (rest files) n))]))

(check-expect (handle-files (dir-files EX) "PC前端.xlsx") #t)
(check-expect (handle-files (dir-files EX) "World.db3") #f)
(check-expect (handle-files (dir-files EX) "abc.efg") #f)


; File String -> Boolean
; determine if file's name is n
(define (handle-file file n)
  (string=? (file-name file) n))

(check-expect (handle-file (make-file "abc.txt" 12 0 "") "a.txt") #f)
(check-expect (handle-file (make-file "abc.txt" 12 0 "") "abc.txt") #t)


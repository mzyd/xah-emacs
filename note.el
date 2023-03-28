;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-o and enter text in its buffer.

(message "Her name is: %s" "Vicky")  ; %s is for string
(message "My list is: %S" (list 8 2 3))  ; %S is for any lisp expression
;; spc m - dired jump
;; m - xah-backward-left-bracket
;; 7 - select line
;; 8 - select word

;; spc , m - eval-last-sexp
;; spc , e - eval-buffer
;; spc o spc - rectangle-mark-mode  ***************

;; spc i e - open file by directory
;; spc k k - repeat
;; spc d k - xah-insert-brace
     
;; spc i d - ibuffer
;; spc i e - find-file
;; spc i j - recentf-open-files
;; spc i l - xah-new-empty-buffer

;; C-o - open

;; m - move to left paren
;; . - move to right paren

;; e - delete backward, r - delete toward

(insert "something")somethingsomethingsomethingsomething

;; When writing a elisp script that does batch processing, it's best to print to your own buffer, because the Messages Buffer scrolls off. 
;; (setq xbuff (generate-new-buffer "*my output*"))
;; (print "something" xbuff)
;; (switch-to-buffer xbuff )

;; (message "some thing or some\nthing")

(length "abc")
(substring "abc123" 0 3)
(concat "some" "thing" " is" " wrong")
(split-string "Hello_Xah_Lee" "_")

(string-to-number "3")
(number-to-string 3)
;; same as number-to-string but can also do format
(format "%d" 3)				;"3"

;; 返回位置 index
(string-match "b+" "abbc") ;; 1
(string-match "b+" "bbc")  ;; 0

(setq xx "swimming in the sea")
;; (string-match "\\[a-z]+?ing\\" xx)
(string-match "\\([a-z]+?ing\\)" xx)
(match-string 1 xx)

(replace-regexp-in-string "</*div>" "<p>" "<div>something</div>")

(replace-regexp-in-string "</div>" "<p>" "<div>something</div>")

(replace-regexp-in-string "<div>" "<p>" "<div>something</div>")

;; *********** regular expression ************

;; to test Regex in elisp
(re-search-forward "[ld]+")

;; or

(let ((case-fold-search nil))
  (re-search-forward "\\.jpg"))
;; cat.jpg
;; abc abc

;; match Newline character and Tab
;; inside elisp string, \t = TAB, \n = newline
;; [\n\t]+ for sequence of {tab, newline, space}

(re-search-forward "[\n\t]+")  ;; dada

;; test word
(re-search-forward "\\[abc\\]")
;; [abcd]
;; [abc]
;; ********** \\ means escape
(re-search-forward "\\[\\]")
;; []

(re-search-forward "(abc)")
;; [abc] abc (abc)

(re-search-forward "\\(d+\\)")
;; dada tada

(re-search-forward "\\1")
;; 1

;; call list-matching-lines and type follow regex
src="\([^"]+?\)"
;; src="cat.jpg"

;; But in lisp code, the same regex needs to have many backslash escapes, like this:
(re-search-forward "src=\"\\([^\"]+?\\)\"" )
;; src="cat.jpg"

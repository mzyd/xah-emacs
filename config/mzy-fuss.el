
(defun remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\^M []))

;;;###autoload
(defun cd-iterm2()
  (interactive)
  (let ((cmd (format "
tell application \"iTerm\"
  activate
  if (count of windows) = 0 then
    set w to (create window with default profile)
  else
    set w to current window
  end if

  tell w
    set targetSession to null

    activate current session
    tell current session of w
      if is at shell prompt then
        set targetSession to current session of w
      end if
    end tell
    if targetSession is null then
      repeat with aTab in tabs
        if targetSession is null then
          tell aTab
            select
            repeat with aSession in sessions
              if targetSession is null then
                tell aSession
                  select
                  if is at shell prompt then
                    set targetSession to aSession
                  end if
                end tell
              end if
            end repeat
          end tell
        end if
      end repeat
    end if
    if targetSession is null then
      create tab with default profile
      -- delay 0.1
      set targetSession to current session of w
    end if

    if targetSession is not null then
      tell targetSession
        select
        set cmd to \"cd \" & quote & \"%s\" & quote & \";clear\"
        write text cmd
      end tell

    end if
  end tell
end tell
" (expand-file-name default-directory))))
    (start-process "cd-iterm2" nil "osascript" "-e" cmd)))

;; (defun my-web-mode-indent-setup ()
;;   (setq web-mode-markup-indent-offset 2) ; web-mode, html tag in html file
;;   (setq web-mode-css-indent-offset 2)    ; web-mode, css in html file
;;   (setq web-mode-code-indent-offset 2)   ; web-mode, js code in html file
;;   )
;; (add-hook 'web-mode-hook 'my-web-mode-indent-setup)


(defun open-emacs-dotfile()
  (interactive)
  (if (file-exists-p "~/.emacs.d/init.el")
      (find-file "~/.emacs.d/init.el")
    (find-file "~/AppData/Roaming/.emacs.d/init.el")))

(defun mzy/insert-something-on-both-sides ()
  "insert something to both sides of your selected region"
  (interactive)
  (let ((s (read-from-minibuffer "Enter your symbol:"))
        (start (region-beginning))
        (end (region-end)))
    (goto-char start)
    (insert s)
    (goto-char (+ end 1))
    (insert s)
    (keyboard-quit)))


(defun mzy/insert-underline-on-both-sides ()
  "insert something to both sides of your selected region"
  (interactive)
  (let ((s "_")
        (start (region-beginning))
        (end (region-end)))
    (goto-char start)
    (insert s)
    (goto-char (+ end 1))
    (insert s)
    (keyboard-quit)))

;; xah-escape
(setq escape-key-sequence '())
(setq escape-timer nil)
(setq first-key "n")
(setq second-key "h")
;; (setq first-key "k")
;; (setq second-key "j")
(defun mzy/escape ()
  (interactive)
  (setq escape-key-sequence (list first-key))
  (insert first-key)
  (setq escape-timer (run-with-timer 0.2 nil (lambda ()
                                               (unless (equal 2 (length escape-key-sequence))
                                                 (progn
                                                   (setq escape-key-sequence '())
                                                   (cancel-timer escape-timer)))))))

(defun mzy/monitor-escape-trigger-key ()
  (interactive)
  (if (equal (nth 0 escape-key-sequence) first-key)
      (progn
        (delete-backward-char 1)
        (setq escape-key-sequence '())
        (xah-fly-command-mode-activate))
    (progn
      (setq escape-key-sequence '())
      (insert second-key))))

;; Simulate pressing o in vim, the func has already bond to the l
(defun mzy/newline ()
  (interactive)
  (if (eolp)
      (progn
        (xah-fly-insert-mode-activate)
        (indent-new-comment-line))
    (progn
      (xah-end-of-line-or-block)
      (xah-fly-insert-mode-activate)
      (indent-new-comment-line))))

(defun mzy/move-to-line-beginning ()
  (interactive)
  (mwim-beginning 0)
  (forward-char (current-indentation)))

(defun mzy/kill-and-edit-line ()
  (interactive)
  (mzy/move-to-line-beginning)
  (kill-line)
  (xah-fly-insert-mode-activate))

(defun mzy/edit-at-point-word ()
  (interactive)
  (thing-cut-word)
  (xah-fly-insert-mode-activate))

(defun mzy/atfd ()
  (interactive)
  (comint-dynamic-list-filename-completions)
  (comint-dynamic-complete-as-filename))

(global-set-key (kbd "C-9") 'mzy/atfd)

(defun mzy/jump-out-pair-and-newline ()
  (interactive)
  (xah-forward-right-bracket)
  (indent-new-comment-line)
  (xah-fly-insert-mode-activate))

;; (defun remember-init ()
;;   "Remember current position and setup."
;;   (interactive)
;;   (point-to-register 8)
;;   )

(defun remember-init ()
  "Remember current position and setup."
  (interactive)
  (point-to-register 8))

(defun remember-jump ()
  "Jump to latest position and setup."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp))
  (message "Have back to remember position"))

(defun mzy-remember-init ()
  "Remember current position and setup."
  (interactive)
  (point-to-register 8))


;; ------------------------------------
;; (setq mzy-register-list '(1 2 3 4))
;; (setq mzy-offset 0)
;; (defun mzy/remember-init ()
;;   (interactive)
;;   (if (> mzy-offset 3)
;;       (setq mzy-offset 0)
;;     (setq mzy-offset (+ mzy-offset 1))
;;     (message "the offset is -- %s" mzy-offset)
;;     )
;;   (point-to-register (nth mzy-offset mzy-register-list)))
;; (defun mzy/remember-jump-previous ()
;;   (interactive)
;;   (if (> mzy-offset 0)
;;       (setq mzy-offset (- mzy-offset 1))
;;     (setq mzy-offset (+ mzy-offset 3)))
;;   (r-jump (nth mzy-offset mzy-register-list))
;;   (message "the previous register is --- %d" mzy-offset))
;; (defun mzy/remember-jump-next ()
;;   (interactive)
;;   (if (< mzy-offset 3)
;;       (setq mzy-offset (+ mzy-offset 1))
;;     (setq mzy-offset 0))
;;   (r-jump (nth mzy-offset mzy-register-list))
;;   (message "the next register is --- %d" mzy-offset))
;; (defun r-jump (r)
;;   "Jump to latest position and setup."
;;   (interactive)
;;   (let ((tmp (point-marker)))
;;     (jump-to-register r)
;;     (set-register r tmp))
;;   (message "Have back to remember position"))
;; ------------------------------------

(defun mzy/copy-and-comment-line ()
  (interactive)
  (xah-copy-line-or-region)
  (previous-line)
  (xah-comment-dwim)
  (open-line 1)
  (xah-paste-or-paste-previous))

(defun mzy/xah-fly-z-key ()
        "key `z'"
        (interactive)
        ;; (cond
        ;; ((eq major-mode 'web-mode) (call-interactively 'web-mode-navigate))
        ;; (t nil)
        (if (eq major-mode 'web-mode)
            (call-interactively 'web-mode-navigate)
          (call-interactively 'xah-goto-matching-bracket)))

(defun mzy/xah-fly-l-key ()
        "key `l'"
        (interactive)
        ;; (cond
        ;; ((eq major-mode 'web-mode) (call-interactively 'web-mode-navigate))
        ;; (t nil)
        (if (eq major-mode 'org-mode)
            (call-interactively 'mzy/org-newline)
          (call-interactively 'mzy/newline)))


(defun mzy/rjsx-comment ()
  (interactive)
  (js-mode)
  (xah-comment-dwim)
  (rjsx-mode))

(defun mzy/xah-fly-semi-key ()
        "key `;'"
        (interactive)
        (if (eq major-mode 'rjsx-mode)
            (call-interactively 'mzy/rjsx-comment)
          (call-interactively 'xah-comment-dwim)))

(defun mzy/org-newline ()
  (interactive)
  (if (eolp)
      (progn
        (org-meta-return)
        (xah-fly-insert-mode-activate))
    (progn
      (xah-end-of-line-or-block)
      (org-meta-return)
      (xah-fly-insert-mode-activate))))

;; (defun mzy/xah-fly-q-key ()
;;         "key `q'"
;;         (interactive)
;;         (view-lossage)
;;         ;; 切换到 `*Help*`，然后获取其内容
;;         (with-current-buffer "*Help*"
;;           (setq buffer-content (buffer-string)))
;;         (setq str (substring buffer-content -86 (length buffer-content)))
;;         (xah-next-window-or-frame)
;;         (quit-window)
;;         (if (string-match "sort-tab-close-current-tab" str)
;;             (progn
;;               (sort-tab-close-current-tab)
;;               (message "有 sort %s" str)
;;               )
;;           (progn
;;             (xah-cut-line-or-region)
;;               (message " Should delete %s" str)
;;             )

(defun mzy/paste-to-next-line ()
  (interactive)
  (if (eolp)
      (progn
        (newline)
        (xah-paste-or-paste-previous))
    (progn
      (mzy/newline)
      (move-beginning-of-line nil)
      (xah-paste-or-paste-previous)
      (xah-fly-command-mode-activate))))

(defun mzy/git-grep-at-point ()
  (interactive)
  (let (word)
    (if (region-active-p)
        (progn
          (setq word (buffer-substring-no-properties (region-beginning) (region-end))))
      (progn
        (setq word (thing-at-point 'word))))
    (helm-grep-do-git-grep word)))
    ;; (counsel-git-grep word)))

;; (defun mzy/kill-ring-buffer ()
  ;; (interactive)
  ;; )

(defun mzy/copy-window-in-another-buffer ()
    (interactive)
    (delete-other-windows)
    (split-window-right))


;; ************ vue *******
(defun mzy/jump-to-script-tag ()
  (interactive)
  (if (search-forward "<scrip" nil t)
      (search-backward "<scrip" nil t)
    (search-backward "<scrip" nil t)
    )
  )

(defun mzy/jump-to-data ()
  (interactive)
  (if (search-forward "data() {" nil t)
      (search-backward "data() {" nil t)
    (search-backward "data() {" nil t)
    )
  )

(defun mzy/jump-to-style ()
  (interactive)
  (if (search-forward "<style" nil t)
      (search-backward "<style" nil t)
    (search-backward "<style" nil t)
    )
  )
(defun mzy/jump-to-methods ()
  (interactive)
  (if (search-forward "methods:" nil t)
      (search-backward "methods:" nil t)
    (search-backward "methods:" nil t)
    )
  )

(defun mzy/switch-to-previous-buffer ()
  "Switch to the previously selected buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; ************ vue *******



(defun get-char-at-position (position)
  (interactive "d")
  (message "***Character at position %d: %c" position (char-after position))
  (message "Character at position %d: %s" position (char-after position))
  ;; (message "00000000 %s" (type-of (char-after position)))
  (char-after position))

;; char-after 返回 Unicode码点值, 是个 integer 类型的数字
;; . 的Unicode码点值是 46
;; right-word 不需要 +1 因为光标就在 region 的右边
(defun mzy/copy-at-point-for-js ()
    (interactive)
    (xah-extend-selection)
    (setq word-left (- (region-beginning) 1))
    ;; (setq word-right (+ (region-end) 1))
    (setq word-right (region-end))

    (if (= (get-char-at-position word-left) 46)
        (progn
          (exchange-point-and-mark)
          (backward-word)))

    (if (= (get-char-at-position word-right) 46)
        (forward-word)
      ))





(provide 'mzy-fuss)

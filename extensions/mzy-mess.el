
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
  (if (= (point) (point-at-eol))
      (progn
        (xah-fly-insert-mode-activate)
        (indent-new-comment-line))
    (progn
      (xah-end-of-line-or-block)
      (xah-fly-insert-mode-activate)
      (indent-new-comment-line)
      )))


(defun mzy/kill-and-edit-line ()
  (interactive)
  (xah-beginning-of-line-or-block)
  (kill-line)
  (xah-fly-insert-mode-init)
  (xah-fly-insert-mode-activate))

(defun mzy/edit-at-point-word ()
  (interactive)
  (edit-at-point-word-cut)
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


(provide 'mzy-mess)

;; M-x package-refresh-contents
(let (;; temporarily increase `gc-cons-threshold' when loading to speed up startup.
      (gc-cons-threshold most-positive-fixnum)
      ;; Empty to avoid analyzing files when loading remote files.
      (file-name-handler-alist nil))
  ;; Emacs configuration file content is written below.
  )

(require 'package)

(setq pacakge-enable-startup nil)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
;; (setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
                         ;; ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
;; (require 'eaf)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-browser)

(add-to-list 'load-path "~/.emacs.d/config/")
(require 'mzy-fuss)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'init-org)
;; (require 'git-timemachine)

(require 'thing-edit)
(require 'xah-fly-keys)

;; specify a layout
(xah-fly-keys-set-layout "dvorak")
;; possible values
;; adnw , azerty , azerty-be , beopy , bepo , carpalx-qfmlwy , carpalx-qgmlwb , carpalx-qgmlwy , colemak , colemak-dhm , colemak-dhm-angle , colemak-dhk , dvorak , koy , neo2 , norman , programer-dvorak , pt-nativo , qwerty , qwerty-abnt , qwerty-no (qwerty Norwegian) , qwertz , workman

(xah-fly-keys 1)

(use-package yasnippet
  :defer t
  :ensure t
  :config
  (yas-global-mode 1))

(use-package markdown-mode
  :defer t
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package posframe
  :ensure t)

;; (desktop-save-mode 1)
(add-to-list 'load-path "~/emacs-packages/lsp-bridge")
;; (if (file-exists-p "~/AppData/Roaming/lsp-bridge")
;;     (add-to-list 'load-path "~/AppData/Roaming/lsp-bridge")
;;   (add-to-list 'load-path "~/emacs-packages/lsp-bridge"))
(require 'lsp-bridge)
(global-lsp-bridge-mode)
;; 当光标悬停在诊断位置时显示诊断工具提示，默认禁用
(setq lsp-bridge-enable-hover-diagnostic t)
(setq acm-enable-tabnine nil)
(setq acm-enable-yas nil)
(setq acm-backend-search-file-words-candidate-min-length 3)
;; (setq acm-backend-lsp-block-kind-list '("Snippet" "Enum"))
;; (setq lsp-bridge-enable-log t)

(require 'sort-tab)
(setq sort-tab-hide-function '(lambda (buf) (with-current-buffer buf (derived-mode-p 'dired-mode))))
(sort-tab-mode 1)

(require 'awesome-tray)
(awesome-tray-mode 1)
(when (not (string= system-type 'gnu/linux))
  (setq awesome-tray-second-line nil)
  (setq awesome-tray-position nil))
;; (setq awesome-tray-separator " 🐸 ")
;; (setq awesome-tray-separator " 🍄 ")
(setq awesome-tray-separator " 🌱 ")
;; 🔧  🍀 🌱 🐞 🍄 🀅 🐸 ❖ ♦

(defface my-module-hello-face
  ;; '((t (:italic t))) ;; 斜体
  '((t))
  "Hello module face."
  :group 'awesome-tray)

(defun my-buffer-modified-status ()
  (if (buffer-modified-p) " 🌈 " ""))

(add-to-list 'awesome-tray-module-alist
           '("my-buffer-modified" . (my-buffer-modified-status my-module-hello-face)))

(setq awesome-tray-active-modules
      '("my-buffer-modified" "location" "git" "date" "mode-name"))

(require 'color-rg)

;; (require 'insert-translated-name)

(global-set-key (kbd "<escape>") 'xah-fly-command-mode-activate)
(define-key xah-fly-insert-map (kbd "C-w") 'backward-kill-word)


;; ******************* 清空键位
(define-key xah-fly-command-map (kbd "'") nil)
(define-key xah-fly-command-map (kbd "e") nil)
(define-key xah-fly-command-map (kbd "m") nil)
(define-key xah-fly-command-map (kbd "v") nil)
(define-key xah-fly-command-map (kbd "l") nil)


(defun mzy/web-newline ()
  (interactive)
  (indent-new-comment-line)
  (previous-line 1)
  (mwim-end)
  ;; (indent-new-comment-line)
  (xah-fly-insert-mode-activate))

(defun my-web-mode-enter ()
  "根据条件执行 mzy/web-newline 或 newline。"
  (interactive)
  (when (and (bound-and-true-p xah-fly-insert-state-p)
             (looking-back "{")
             (looking-at "}"))
    (mzy/web-newline)
    (message "Executed mzy/web-newline"))
  (unless (and (bound-and-true-p xah-fly-insert-state-p)
               (looking-back "{")
               (looking-at "}"))
    (indent-new-comment-line)))

;; 将函数绑定到 web-mode 的回车键
(add-hook 'web-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'my-web-mode-enter)))

;; auto save
(require 'auto-save)
(auto-save-enable)
(setq auto-save-silent t)   ; quietly save
(setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving

;; custom predicates if you don't want auto save.
;; disable auto save mode when current filetype is an vue file.
(setq auto-save-disable-predicates
      '((lambda ()
          (string-suffix-p
           "vue"
           (file-name-extension (buffer-name)) t))
        (lambda ()
          (string-suffix-p
           "js"
           (file-name-extension (buffer-name)) t))
        (lambda ()
          (string-suffix-p
           "el"
           (file-name-extension (buffer-name)) t))
        (lambda ()
          (string-suffix-p
           "ts"
           (file-name-extension (buffer-name)) t))
        ))

(require 'highlight-matching-tag)
(highlight-matching-tag 1)

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(if (eq system-type 'windows-nt)
    (progn
      (global-set-key (kbd "M-<up>") 'git-gutter:previous-hunk)
      (global-set-key (kbd "M-<down>") 'git-gutter:next-hunk)
      ))

(add-to-list 'load-path "~/emacs-packages/holo-layer")
(require 'holo-layer)
(setq holo-layer-enable-cursor-animation t)
;; (setq holo-layer-cursor-color "#FF7145") ; 橙色
(setq holo-layer-cursor-color "#10ff00") ; 绿色
(setq holo-layer-cursor-alpha 100)
;; (setq holo-layer-enable-window-border nil)
(add-to-list 'holo-layer-cursor-block-commands "self-insert-command")
(holo-layer-enable)

(unless (eq system-type 'windows-nt)
  (progn
    (add-to-list 'load-path "~/emacs-packages/blink-search")
    (require 'blink-search)
    (setq blink-search-enable-posframe t)
    (define-key blink-search-mode-map (kbd "<up>") 'blink-search-candidate-select-prev)
    (define-key blink-search-mode-map (kbd "<down>") 'blink-search-candidate-select-next)
    (define-key blink-search-mode-map (kbd "C-b") 'blink-search-parent)
    ))

(defun my-blink-search ()
    (interactive)
    (blink-search)
    (xah-fly-insert-mode-activate))


;; 看过文档再定义键位
;; (require 'markmacro)
;; (global-set-key (kbd "M-w m w") 'markmacro-mark-words)
;; (global-set-key (kbd "M-w m l") 'markmacro-mark-lines)
;; (global-set-key (kbd "M-w m c") 'markmacro-mark-chars)
;; (global-set-key (kbd "M-w m i") 'markmacro-mark-imenus)
;; (global-set-key (kbd "M-w a") 'markmacro-apply-all)
;; (global-set-key (kbd "M-w e") 'markmacro-apply-all-except-first)
;; (global-set-key (kbd "M-w r s") 'markmacro-rect-set)
;; (global-set-key (kbd "M-w r d") 'markmacro-rect-delete)
;; (global-set-key (kbd "M-w r r") 'markmacro-rect-replace)
;; (global-set-key (kbd "M-w r i") 'markmacro-rect-insert)
;; (global-set-key (kbd "M-w m o") 'markmacro-rect-mark-columns)
;; (global-set-key (kbd "M-w m s") 'markmacro-rect-mark-symbols)

;; ========================== basic setting =============================
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 160 :weight normal :family "Source Code Pro")))))
;; '(default ((t (:height 160 :weight normal :family "DinaRemaster")))))
;; '(default ((t (:height 160 :weight normal :family "Anonymous Pro")))))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; 启用 fringe, 这里 3 是像素宽度
(fringe-mode 4)

(setq tool-bar-mode nil)
(setq menu-bar-mode nil)

;; 高亮当前行
(global-hl-line-mode 1)
(setq ring-bell-function 'ignore)
;; (set-background-color "#F0FFF2")
(fset 'yes-or-no-p 'y-or-n-p)

(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; (define-key dired-mode-map (kbd "s-b") 'dired-up-directory)

;; org 中文换行问题
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(setq hippie-expand-try-functions-list
      '(
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        ;; try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name
        ;; try-expand-all-abbrevs
        ;; try-expand-list
        ;; try-expand-line
        ))

;; set language environment
(set-language-environment 'UTF-8)
(set-locale-environment "UTF-8")

(use-package material-theme
  :ensure t
  :config
  (load-theme 'material-light t)
  (set-cursor-color "#0011ff"); 蓝色
  (setq-default mode-line-format nil))


;; =================== packages ===================

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t))

(use-package highlight-parentheses
  :ensure t
  :config
  (global-highlight-parentheses-mode t))

(sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(require 'cache-path-from-shell)

(use-package which-key
  :ensure t
  :config (which-key-mode)
  (setq which-key-idle-delay 0.1))

;; (use-package restart-emacs
;; :ensure t)
(use-package better-defaults
  :ensure t)

;; org-mode stuff
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package eslint-fix
  :ensure t
  :config
  (add-hook 'js-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t)))
  ;; (add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t)))
  (add-hook 'web-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t)))
  (add-hook 'typescript-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))

;; (define-key org-mode-map (kbd "<return>") 'org-meta-return)

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1))

(use-package counsel
  :ensure t)

;; (use-package dot-mode
;;   :ensure t
;;   :config
;;   (global-dot-mode t))

;; you also can pressing d in command mode to excute this function
;; (defun mzy/dot-mode-excute ()
;;   (interactive)
;;   (xah-fly-insert-mode-activate)
;;   (dot-mode-execute)
;;   (xah-fly-command-mode-activate))

;; (use-package avy
;;   :ensure t)


(use-package youdao-dictionary
  :ensure t)
(setq youdao-dictionary-app-key "69da6938ff66ae77")
(setq youdao-dictionary-secret-key "MVCHnxojn19AFwUpyiuHUtaQcbHj9s1b")

(setq url-automatic-caching t)

(unless (eq system-type 'windows-nt)
  (use-package fasd
    :ensure t
    :config
    (global-fasd-mode 1)))

(use-package mwim
  :ensure t
  :bind ("C-a" . mwim-beginning)
  :bind ("C-e" . mwim-end))


(define-key xah-fly-command-map (kbd "m") nil)
(define-key xah-fly-command-map (kbd "v") nil)
(use-package symbol-overlay
  :ensure t
  :config
  (setq symbol-overlay-map (make-sparse-keymap)) ; 直接清空键映射
  :bind
  (:map xah-fly-command-map
        ("m" . symbol-overlay-jump-prev)
        ("v" . symbol-overlay-jump-next)))

(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    ;; enable this if you want `swiper' to use it
    ;; (setq search-default-mode #'char-fold-to-regexp)
    (define-key xah-fly-command-map (kbd "b") 'swiper)
    ;; (define-key xah-fly-command-map (kbd "n") 'swiper)
    ))

(use-package find-file-in-project
  :ensure t)

;; (use-package git-gutter
;;   :ensure t
;;   ;; :bind ("M-<up>" . git-gutter:previous-hunk)
;;   ;; :bind ("M-<down>" . git-gutter:next-hunk)
;;   :config
;;   (progn
;;     (global-git-gutter-mode -1)
;;     (custom-set-variables
;;      '(git-gutter:window-width 2)
;;      '(git-gutter:modified-sign "♣ ")
;;      '(git-gutter:added-sign "♦ ")
;;      '(git-gutter:deleted-sign "✘ ")
;;      ;; ♥
;;      '(git-gutter:lighter " ✡"))
;;     ;; (set-face-background 'git-gutter:modified "yellow") ;; background color
;;     (set-face-foreground 'git-gutter:modified "black")
;;     (set-face-foreground 'git-gutter:added "green")
;;     (set-face-foreground 'git-gutter:deleted "#cc0000")
;;     ))
;; (when (eq system-type 'windows-nt)
  ;; (global-set-key (kbd "M-<up>") 'git-gutter:previous-hunk)
  ;; (global-set-key (kbd "M-<down>") 'git-gutter:next-hunk))

(use-package diff-hl
  :ensure t
  :bind (("M-<up>" . diff-hl-previous-hunk)
         ("M-<down>" . diff-hl-next-hunk)
         ("C-c C-g" . diff-hl-update))
  :hook ((after-save . diff-hl-update)
         (magit-post-refresh . diff-hl-update)
         (find-file . (lambda ()
                       (when (vc-git-root default-directory)
                         (diff-hl-update))))
         (dired-mode . diff-hl-dired-mode))
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode t)
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1)))

(global-set-key (kbd "C-c C-s") 'diff-hl-show-hunk)

;; (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)


(when (eq system-type 'darwin)
  (require 'mac-key-mode)
  (mac-key-mode t))

(require 'kill-ring-search)
(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer."
  (interactive))

;; (use-package magit
;;   :ensure t
;;   :bind (("C-x g" . magit-status)
;;          ("C-x C-g" . magit-status)))

;; the global-linum-mode must be placed behind git-gutter
;; even that, there's still have a problem: when you changed something, you have to move your cursor over the screen
;; and the git-gutter will refresh its state, else, it won't fucking do anything. of course, this is the god damn linum-mode's problem.

;; (use-package nyan-mode
;;   :ensure t
;;   :config (nyan-mode 1))

(require 'zone)
(zone-when-idle 6000)


;; (use-package company
;;   :ensure)
;; (global-company-mode t)
;; (setq company-idle-delay 0)
;; (setq company-minimum-prefix-length 1)

;; (use-package flycheck
;;   :ensure t
;;   :config
;;   (add-hook 'after-init-hook #'global-flycheck-mode))
;; (setq lsp-ui-sideline-show-diagnostics t)

(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2))

(use-package web-mode
  :ensure t
  :config
  ;; (add-hook 'css-mode-hook 'rainbow-mode)
  (setq web-mode-markup-indent-offset 2) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset 2)    ; web-mode, css in html file
  (setq web-mode-code-indent-offset 2)   ; web-mode, js code in html file
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0)
  (setq auto-mode-alist
        (append
         '(("\\.js\\'" . javascript-mode))
         '(("\\.html\\'" . web-mode))
         '(("\\.wxml\\'" . web-mode))
         '(("\\.css\\'" . css-mode))
         '(("\\.vue\\'" . web-mode))
         '(("\\.tsx\\'" . web-mode))
         '(("\\.hbs\\'" . web-mode))
         auto-mode-alist)))

(setq js-indent-level 2)

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'html-mode-hook 'emmet-mode)
  ;; (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))
(define-key web-mode-map (kbd "C-c c") 'emmet-expand-yas)

(use-package smex
  :ensure t)

(use-package edit-at-point
  :ensure)

(use-package goto-line-preview
  :ensure)

;; react mode
(use-package rjsx-mode
  :ensure)

;; org-mode 插入图片
(use-package org-download
  :ensure
  :config
  (setq-default org-download-heading-lvl nil)
  (setq-default org-download-image-dir "./images")
  (setq org-download-backend "wget")
  (setq org-download-abbreviate-filename-function (lambda (fn) fn)) ; use original filename
  (defun dummy-org-download-annotate-function (link)
    "")
  (setq org-download-annotate-function
      #'dummy-org-download-annotate-function))

(add-hook 'dired-mode-hook 'org-download-enable)

;; 代替 dired-mode
;; (use-package dirvish
;;   :ensure)

;; ********************************************

(defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
  "Workaround sgml-mode and follow airbnb component style."
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "^ +\/?> *$")
        (delete-char sgml-basic-offset))))

;; 在 insert-mode 里刷键 nh 即可退出 insert-mode
(add-hook 'xah-fly-insert-mode-activate-hook (lambda ()
                                               (define-key xah-fly-insert-map (kbd first-key) 'mzy/escape)
                                               (define-key xah-fly-insert-map (kbd second-key) 'mzy/monitor-escape-trigger-key)))

(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2
                                          js-indent-level 2
                                          js2-strict-missing-semi-warning nil)))

(defun mzy/wrap-with-html-tag (tag-name)
  "Wrap the selected region with specified HTML tag.
If region is not active, do nothing.
TAG-NAME is the HTML tag name without angle brackets."
  (interactive "sEnter tag name (without <>): ")
  (if (use-region-p)
      (let* ((region-beginning (region-beginning))
             (region-end (region-end))
             (tag-name (downcase (string-trim tag-name)))
             (has-attributes (string-match-p "\\s-+" tag-name))
             (base-tag (if has-attributes
                          (car (split-string tag-name "\\s-+"))
                        tag-name))
             (attributes (if has-attributes
                           (concat " " (mapconcat 'identity (cdr (split-string tag-name "\\s-+")) " "))
                         ""))
             (opening-tag (format "<%s%s>" base-tag attributes))
             (closing-tag (format "</%s>" base-tag)))
        ;; Insert tags around region
        (save-excursion
          (goto-char region-end)
          (insert closing-tag)
          (goto-char region-beginning)
          (insert opening-tag)))
    (message "No region selected!")))

(defun mzy/wrap-line-with-li ()
  "Wrap the current line with <li> tags.
Removes leading and trailing whitespace before wrapping.
If the line is empty or contains only whitespace, do nothing."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let* ((line-start (point))
           ;; Get end of line position
           (line-end (progn (end-of-line) (point)))
           ;; Get line content
           (line-content (buffer-substring-no-properties line-start line-end))
           ;; Trim whitespace and check if line is empty
           (trimmed-content (string-trim line-content))
           (line-empty (string-empty-p trimmed-content)))
      (if (not line-empty)
          (progn
            ;; Delete the original line content
            (delete-region line-start line-end)
            ;; Insert the trimmed content with li tags
            (insert (concat "<li>" trimmed-content "</li>")))
        (message "Current line is empty!")))))

(defun wrap-line-with-li ()
  "Wrap the current line with <li> tags.
Removes leading and trailing whitespace before wrapping.
If the line is empty or contains only whitespace, skip it."
  (save-excursion
    (beginning-of-line)
    (let* ((line-start (point))
           (line-end (progn (end-of-line) (point)))
           (line-content (buffer-substring-no-properties line-start line-end))
           (trimmed-content (string-trim line-content))
           (line-empty (string-empty-p trimmed-content)))
      (unless line-empty
        (delete-region line-start line-end)
        (insert (concat "<li>" trimmed-content "</li>"))))))


(defun mzy/wrap-region-with-li ()
  "Wrap each non-empty line in the selected region with <li> tags.
If no region is selected, operate on current line only."
  (interactive)
  (if (use-region-p)
      (let ((start-line (line-number-at-pos (region-beginning)))
            (end-line (line-number-at-pos (region-end))))
        (save-excursion
          ;; Go to first line of region
          (goto-char (region-beginning))
          (beginning-of-line)
          ;; Process each line in region
          (while (<= (line-number-at-pos) end-line)
            (wrap-line-with-li)
            (forward-line 1))))
    ;; If no region selected, just process current line
    (wrap-line-with-li)))

;; Optional: Bind the functions to keys
;; (global-set-key (kbd "C-c l") 'wrap-line-with-li)      ; 单行
;; (global-set-key (kbd "C-c L") 'wrap-region-with-li)    ; 多行


(defun remove-html-tags-and-colons ()
  "Remove HTML tags and colons from the selected region."
  (interactive)
  (when (use-region-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (save-excursion
        ;; 去除 HTML 标签
        (goto-char start)
        (while (re-search-forward "<[^>]*>" end t)
          (replace-match "" nil nil))

        ;; 去除冒号
        (goto-char start)
        (while (re-search-forward ":" end t)
          (replace-match "" nil nil))

        ;; 去除多余的空白
        (goto-char start)
        (while (re-search-forward "[ \t]+" end t)
          (replace-match " " nil nil))))))

(defun remove-newlines-and-spaces ()
  "Remove all newlines and spaces from the selected region."
  (interactive)
  (when (use-region-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (save-excursion
        ;; 去除所有换行
        (goto-char start)
        (while (re-search-forward "\n" end t)
          (replace-match "" nil nil))

        ;; 去除所有空格（包括空白字符）
        (goto-char start)
        (while (re-search-forward "[ \t]+" end t)
          (replace-match "" nil nil))))))


;; ****************** key binding *********************

(defun mzy/delete-current-letter-and-insert ()
  (interactive)
  (delete-char 1)
  (xah-fly-insert-mode-activate))


;; ************ 键位映射
(define-key xah-fly-command-map (kbd "l") 'mzy/xah-fly-l-key)
(define-key xah-fly-command-map (kbd "e d") 'symbol-overlay-jump-to-definition)
(define-key xah-fly-command-map (kbd "e l") 'mzy/kill-and-edit-line)
(define-key xah-fly-command-map (kbd "e m") 'remember-init)
(define-key xah-fly-command-map (kbd "e e") 'remember-jump)
(define-key xah-fly-command-map (kbd "e a") 'mzy/delete-current-letter-and-insert)
;; (define-key xah-fly-command-map (kbd "' m") 'mzy/remember-init)
;; (define-key xah-fly-command-map (kbd "' <up>") 'mzy/remember-jump-previous)
;; (define-key xah-fly-command-map (kbd "' <down>") 'mzy/remember-jump-next)

;; (define-key xah-fly-command-map (kbd "' r") 'xah-reformat-lines)
(define-key xah-fly-command-map (kbd "e c c") 'thing-copy-parentheses)
(define-key xah-fly-command-map (kbd "e c x") 'thing-cut-parentheses)
(define-key xah-fly-command-map (kbd "e c t") 'mzy/wrap-with-html-tag)
(define-key xah-fly-command-map (kbd "e c l") 'mzy/wrap-line-with-li)
(define-key xah-fly-command-map (kbd "e c a") 'mzy/wrap-region-with-li)
(define-key xah-fly-command-map (kbd "e c a") 'mzy/wrap-region-with-li)
(define-key xah-fly-command-map (kbd "e c m") 'remove-html-tags-and-colons)
(define-key xah-fly-command-map (kbd "e c r") 'remove-newlines-and-spaces)




(define-key xah-fly-command-map (kbd "e <DEL>") 'thing-cut-word)
(define-key xah-fly-command-map (kbd "e w") 'mzy/edit-at-point-word)
(define-key xah-fly-command-map (kbd "e r") 'symbol-overlay-query-replace)

(define-key xah-fly-command-map (kbd "' - -") 'mzy/insert-underline-on-both-sides)
(define-key xah-fly-command-map (kbd "' w w") 'mzy/copy-window-in-another-buffer)

(define-key xah-fly-command-map (kbd "e s s") 'counsel-git-grep)
(define-key xah-fly-command-map (kbd "e s c") 'mzy/git-grep-at-point)
(define-key xah-fly-command-map (kbd "e s l") 'eslint-fix)
(define-key xah-fly-command-map (kbd "e s w") 'youdao-dictionary-search-at-point-tooltip)

(defun mzy/xah-fly-ej-key ()
  (interactive)
  (when (and buffer-file-name (string= (file-name-extension buffer-file-name) "vue") (derived-mode-p 'web-mode))
    (define-key xah-fly-command-map (kbd "e j d") 'mzy/jump-to-data)
    (define-key xah-fly-command-map (kbd "e j s") 'mzy/jump-to-style)
    (define-key xah-fly-command-map (kbd "e j m") 'mzy/jump-to-methods)
    (define-key xah-fly-command-map (kbd "e j j") 'mzy/jump-to-script-tag)))

(add-hook 'web-mode-hook #'mzy/xah-fly-ej-key)

(define-key xah-fly-command-map (kbd "e 8") 'mzy/copy-at-point-for-js)
(define-key xah-fly-command-map (kbd "e y") 'helm-show-kill-ring)
(define-key xah-fly-command-map (kbd "e TAB") 'switch-to-previous-buffer)
(define-key xah-fly-command-map (kbd "e o v") 'org-download-clipboard)

;; vundo
;; f   to go forward
;; b   to go backward

;; n   to go to the node below when you at a branching point
;; p   to go to the node above

;; a   to go back to the last branching point
;; e   to go forward to the end/tip of the branch

;; q   to quit, you can also type C-g

;; C-c C-s (or whatever binding you used for save-buffer)
;;     to save the buffer at the current undo state
(define-key xah-fly-command-map (kbd "e f") 'vundo)

(define-key xah-fly-command-map (kbd "e 3 3") 'color-rg-search-input)
(define-key xah-fly-command-map (kbd "e 3 s") 'color-rg-search-symbol)
(define-key xah-fly-command-map (kbd "e 3 8") 'color-rg-search-input-in-project)

(define-key xah-fly-command-map (kbd "e <up>") 'xah-backward-left-bracket)
(define-key xah-fly-command-map (kbd "e <down>") 'xah-forward-right-bracket)

;; sort-tab
(define-key xah-fly-command-map (kbd "<right>") 'sort-tab-select-next-tab)
(define-key xah-fly-command-map (kbd "<left>") 'sort-tab-select-prev-tab)
(define-key xah-fly-command-map (kbd "5") 'sort-tab-close-current-tab)
(define-key xah-fly-command-map (kbd "e 5") 'sort-tab-close-all-tabs)

;; (define-key xah-fly-command-map (kbd "e t d") 'lsp-bridge-find-def-other-window)

;; (define-key xah-fly-command-map (kbd "e c n") 'insert-translated-name-insert)

;; EEEEEEE
;; (define-key xah-fly-command-map (kbd "e c") 'mzy/remember-jump-previous)
;; (define-key xah-fly-command-map (kbd "e t") 'mzy/remember-jump-next)


;; (fset 'delete-empty-lines (kbd "M-x flush-lines RET ^\s-*$ RET"))

(global-set-key (kbd "C-8") 'nil)
(define-key xah-fly-insert-map (kbd "C-8 d") 'lsp-bridge-find-def)
(define-key xah-fly-insert-map (kbd "C-8 t") 'lsp-bridge-find-type-def)
(define-key xah-fly-insert-map (kbd "C-8 n") 'lsp-bridge-diagnostic-jump-next)
(define-key xah-fly-insert-map (kbd "C-8 p") 'lsp-bridge-diagnostic-jump-prev)
(define-key xah-fly-insert-map (kbd "C-8 l") 'lsp-bridge-diagnostic-list)
(define-key xah-fly-insert-map (kbd "C-8 c") 'lsp-bridge-diagnostic-copy)

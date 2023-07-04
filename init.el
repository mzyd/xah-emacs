;; M-x package-refresh-contents

(require 'package)

(setq pacakge-enable-startup nil)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)


(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'load-path "~/.emacs.d/config/")
(require 'mzy-fuss)

(add-to-list 'load-path "~/.emacs.d/lisp/")
;; (require 'git-timemachine)

(require 'thing-edit)
(require 'xah-fly-keys)

;; specify a layout
(xah-fly-keys-set-layout "dvorak")
;; possible values
;; adnw , azerty , azerty-be , beopy , bepo , carpalx-qfmlwy , carpalx-qgmlwb , carpalx-qgmlwy , colemak , colemak-dhm , colemak-dhm-angle , colemak-dhk , dvorak , koy , neo2 , norman , programer-dvorak , pt-nativo , qwerty , qwerty-abnt , qwerty-no (qwerty Norwegian) , qwertz , workman

(xah-fly-keys 1)

(global-set-key (kbd "<escape>") 'xah-fly-command-mode-activate)
(define-key xah-fly-insert-map (kbd "C-w") 'backward-kill-word)

;; (define-key git-timemachine-mode-map (kbd "C-p") git-timemachine-visit)
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
        ))

(require 'highlight-matching-tag)
(highlight-matching-tag 1)

;; (require 'awesome-tray)
;; (awesome-tray-mode 1)
;; (setq awesome-tray-second-line nil)
;; (setq awesome-tray-position nil)

;; (require 'init-session)
;; (emacs-session-restore)

;; (add-hook 'kill-emacs-hook #'emacs-session-save)
;; (add-hook 'kill-emacs-hook (lambda ()
                             ;; (emacs-session-save)
;; ))

(unless (eq system-type 'windows-nt)
  (progn
    (add-to-list 'load-path "~/blink-search")
    (require 'blink-search)))

(require 'markmacro)
(global-set-key (kbd "M-w m w") 'markmacro-mark-words)
(global-set-key (kbd "M-w m l") 'markmacro-mark-lines)
(global-set-key (kbd "M-w m c") 'markmacro-mark-chars)
(global-set-key (kbd "M-w m i") 'markmacro-mark-imenus)
(global-set-key (kbd "M-w a") 'markmacro-apply-all)
(global-set-key (kbd "M-w e") 'markmacro-apply-all-except-first)
(global-set-key (kbd "M-w r s") 'markmacro-rect-set)
(global-set-key (kbd "M-w r d") 'markmacro-rect-delete)
(global-set-key (kbd "M-w r r") 'markmacro-rect-replace)
(global-set-key (kbd "M-w r i") 'markmacro-rect-insert)
(global-set-key (kbd "M-w m o") 'markmacro-rect-mark-columns)
(global-set-key (kbd "M-w m s") 'markmacro-rect-mark-symbols)

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
(fringe-mode 0)
(global-hl-line-mode 1)
(setq ring-bell-function 'ignore)
;; (set-background-color "#F0FFF2")
(fset 'yes-or-no-p 'y-or-n-p)

(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; (define-key dired-mode-map (kbd "s-b") 'dired-up-directory)

;; org 中文换行问题
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

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

(use-package exec-path-from-shell
  :ensure t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package which-key
  :ensure t
  :config (which-key-mode)
  (setq which-key-idle-delay 0.1))

(use-package restart-emacs
  :ensure t)

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
  ;; (add-hook 'web-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t)))
  ;; (add-hook 'vue-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t)))
  )

;; (define-key org-mode-map (kbd "<return>") 'org-meta-return)

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1))

(use-package counsel
  :ensure t)

(use-package dot-mode
  :ensure t
  :config
  (global-dot-mode t))

;; you also can pressing d in command mode to excute this function
(defun mzy/dot-mode-excute ()
  (interactive)
  (xah-fly-insert-mode-activate)
  (dot-mode-execute)
  (xah-fly-command-mode-activate))

;; (use-package avy
;;   :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (add-to-list 'load-path "~/.emacs.d/yasnippet")
  (yas-global-mode 1))

(use-package youdao-dictionary
  :ensure t)

(unless (eq system-type 'windows-nt)
  (use-package fasd
  :ensure t
  :config
  (global-fasd-mode 1)))

(use-package mwim
  :ensure t
  :bind ("C-a" . mwim-beginning)
  :bind ("C-e" . mwim-end))

;; (use-package company
;;   :ensure t
;;   :config
;;   (global-company-mode t)
;;   (setq company-prefix 1)
;;   (setq company-idle-delay 0.1)
;;   (setq company-minimum-prefix-length 1)
;;   :bind
;;   (:map company-active-map
;;         ([tab] . smarter-yas-expand-next-field-complete)
;;         ("TAB" . smarter-yas-expand-next-field-complete)))
;; (use-package auto-complete
;;   :ensure t
;;   :config
;;   (ac-config-default)
;;   (setq ac-auto-show-menu 0.02)
;;   (setq ac-use-menu-map t)
;;   (define-key ac-menu-map "C-n" 'ac-next)
;;   (define-key ac-menu-map "C-p" 'ac-previous))
;; (use-package flycheck
;;   :ensure t
;;   :config
;;   (global-flycheck-mode))
;; (use-package flycheck-posframe
;;   :ensure t
;;   :after flycheck
;;   :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(use-package symbol-overlay
  :ensure t
  :config
  :bind ("M-i" . symbol-overlay-put)
  :bind ("M-t" . symbol-overlay-jump-next)
  :bind ("M-c" . symbol-overlay-jump-prev))

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

(use-package git-gutter
  :ensure t
  ;; :bind ("M-<up>" . git-gutter:previous-hunk)
  ;; :bind ("M-<down>" . git-gutter:next-hunk)
  :config
  (progn
    (global-git-gutter-mode +1)
    (custom-set-variables
     '(git-gutter:window-width 2)
     '(git-gutter:modified-sign "♣ ")
     '(git-gutter:added-sign "♦ ")
     '(git-gutter:deleted-sign "✘ ")
     ;; ♥
     '(git-gutter:lighter " ✡"))
    ;; (set-face-background 'git-gutter:modified "yellow") ;; background color
    (set-face-foreground 'git-gutter:modified "black")
    (set-face-foreground 'git-gutter:added "green")
    (set-face-foreground 'git-gutter:deleted "#cc0000")
    ))


(when (eq system-type 'windows-nt)
  (global-set-key (kbd "M-<up>") 'git-gutter:previous-hunk)
  (global-set-key (kbd "M-<down>") 'git-gutter:next-hunk))

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
;; (global-linum-mode 1)
;; (setq linum-format " %d ")

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

;; (use-package nyan-mode
;;   :ensure t
;;   :config (nyan-mode 1))

(require 'zone)
(zone-when-idle 600)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package posframe
  :ensure t)

(if (file-exists-p "~/AppData/Roaming/lsp-bridge")
    (add-to-list 'load-path "~/AppData/Roaming/lsp-bridge")
  (add-to-list 'load-path "~/lsp-bridge"))
(require 'lsp-bridge)
(global-lsp-bridge-mode)
;; (setq lsp-bridge-enable-auto-format-code t)
;; (setq acm-completion-backend-merge-order '("template-first-part-candidates" "mode-first-part-candidates" "tabnine-candidates" "template-second-part-candidates" "mode-second-part-candidates"))


(use-package typescript-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  ;; (add-hook 'css-mode-hook 'rainbow-mode)
  (setq web-mode-markup-indent-offset 2) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset 2)    ; web-mode, css in html file
  (setq web-mode-code-indent-offset 2)   ; web-mode, js code in html file
  (setq auto-mode-alist
        (append
         '(("\\.js\\'" . javascript-mode))
         '(("\\.html\\'" . web-mode))
         '(("\\.wxml\\'" . web-mode))
         '(("\\.css\\'" . scss-mode))
         '(("\\.vue\\'" . web-mode))
         auto-mode-alist)))

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))
(define-key web-mode-map (kbd "C-c c") 'emmet-expand-yas)

(use-package scss-mode
  :ensure t
  :config
  (setq css-indent-offset 2)
  (add-hook 'css-mode-hook
            '(lambda()
               (setq tab-width 4))))

(use-package vue-mode
  :ensure t)

(use-package smex
  :ensure t)

;; (use-package copilot
;;   :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;;   :ensure t)

;; (add-to-list 'load-path "~/emacs-application-framework/")
;; (require 'eaf)
;; (require 'eaf-browser)

(use-package edit-at-point
  :ensure)

(use-package goto-line-preview
    :ensure)

(when (eq system-type 'darwin)
      (add-to-list 'load-path "~/org-ai")
      (require 'org)
      (require 'org-ai)
      (add-hook 'org-mode-hook #'org-ai-mode)
      (org-ai-global-mode)
      (setq org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
      (org-ai-install-yasnippets) ; if you are using yasnippet and want `ai` snippets
      ;; (setq org-ai-openai-api-token "w0q5UCuXjCeyYA5I124mT3BlbkFJNUPlGrrHgOiGX3zp7M29")
      (setq org-ai-openai-api-token "IizswsaiGCmM7vD8XyM8T3BlbkFJGpYOCsU2rMTtoTGsS9BI"))

(add-hook 'xah-fly-insert-mode-activate-hook (lambda ()
                                               (define-key xah-fly-insert-map (kbd first-key) 'mzy/escape)
                                               (define-key xah-fly-insert-map (kbd second-key) 'mzy/monitor-escape-trigger-key)))

(add-hook 'xah-fly-insert-mode-activate-hook #'remember-init)

;; (defun mzy/lsp-bridge-is-xah-command-state ()
;;   "If `xah-command' mode is enable, only show completion when xah-fly-keys is in insert mode."
;;   (interactive)
;;   (if xah-fly-insert-state-p
;;       t
;;     nil)
;;   )
;; (setq lsp-bridge-completion-popup-predicates '(mzy/lsp-bridge-is-xah-command-state lsp-bridge-not-follow-complete lsp-bridge-not-match-stop-commands lsp-bridge-not-match-hide-characters lsp-bridge-not-only-blank-before-cursor lsp-bridge-not-in-string lsp-bridge-not-in-org-table lsp-bridge-not-execute-macro lsp-bridge-not-in-multiple-cursors lsp-bridge-not-in-mark-macro lsp-bridge-is-evil-state lsp-bridge-is-meow-state lsp-bridge-not-complete-manually))


(define-key xah-fly-command-map (kbd "'") nil)
(define-key xah-fly-command-map (kbd "' d") 'symbol-overlay-jump-to-definition)
(define-key xah-fly-command-map (kbd "' l") 'mzy/kill-and-edit-line)
(define-key xah-fly-command-map (kbd "' m") 'remember-init)
(define-key xah-fly-command-map (kbd "' '") 'remember-jump)
(define-key xah-fly-command-map (kbd "' r") 'xah-reformat-lines)
(define-key xah-fly-command-map (kbd "' w") 'mzy/edit-at-point-word)
(define-key xah-fly-command-map (kbd "' c c") 'thing-copy-parentheses)
(define-key xah-fly-command-map (kbd "' c d") 'thing-cut-word)
(define-key xah-fly-command-map (kbd "' p") 'symbol-overlay-query-replace)

(define-key xah-fly-command-map (kbd "' 8 3") 'mzy/copy-window-in-another-buffer)


(require 'package)

(setq pacakge-enable-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'xah-fly-keys)

;; specify a layout
(xah-fly-keys-set-layout "qwerty")

;; possible values
;; adnw , azerty , azerty-be , beopy , bepo , carpalx-qfmlwy , carpalx-qgmlwb , carpalx-qgmlwy , colemak , colemak-dhm , colemak-dhm-angle , colemak-dhk , dvorak , koy , neo2 , norman , programer-dvorak , pt-nativo , qwerty , qwerty-abnt , qwerty-no (qwerty Norwegian) , qwertz , workman

(xah-fly-keys 1)

(global-set-key (kbd "<escape>") 'xah-fly-command-mode-activate)
(define-key xah-fly-insert-map (kbd "C-w") 'backward-kill-word)

;; ========================== basic setting =============================
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 160 :weight normal :family "Source Code Pro")))))

(fringe-mode 0)
(global-hl-line-mode 1)
(setq ring-bell-function 'ignore)
;; (set-background-color "#F0FFF2")
(fset 'yes-or-no-p 'y-or-n-p)

(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; =================== packages ===================

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t))

(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package which-key
  :ensure t
  :config (which-key-mode)
  (setq which-key-idle-delay 0.1))

;; (use-package restart-emacs
  ;; :ensure t)

(use-package better-defaults
  :ensure t)

(use-package counsel
  :ensure t)

;; (use-package avy
;;   :ensure t)

;; (use-package recentf
;;   :ensure t
;;   :config
;;   (recentf-mode 1)
;;   (setq recentf-max-menu-item 20))

(use-package yasnippet
  :ensure t
  :config
  (add-to-list 'load-path "~/.emacs.d/yasnippet")
  (yas-global-mode 1))

(use-package youdao-dictionary
  :ensure t)

(use-package fasd
  :ensure t
  :init (global-fasd-mode 1))

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
  :bind ("M-n" . symbol-overlay-jump-next)
  :bind ("M-p" . symbol-overlay-jump-prev))
(define-key xah-fly-command-map (kbd "C-w 8") 'symbol-overlay-remove-all)

(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    ;; enable this if you want `swiper' to use it
    ;; (setq search-default-mode #'char-fold-to-regexp)
    ;; (define-key xah-fly-command-map (kbd "n") 'swiper)
    ))
;; (define-key xah-fly-command-map (kbd "Spc w o") 'counsel-git)
(define-key xah-fly-command-map (kbd "n") 'swiper-thing-at-point)
(define-key xah-fly-insert-map (kbd "C-;") 'xah-fly-command-mode-activate)

(use-package git-gutter
  :ensure t)

(global-git-gutter-mode +1)
(custom-set-variables
 '(git-gutter:window-width 2)
 '(git-gutter:modified-sign "♣ ")
 '(git-gutter:added-sign "♦ ")
 '(git-gutter:deleted-sign "✘ ")
 '(git-gutter:lighter "GG")
 )
;; (set-face-background 'git-gutter:modified "yellow") ;; background color
(set-face-foreground 'git-gutter:modified "black")
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "#cc0000")

;; the global-linum-mode must be placed behind git-gutter
;; even that, there's still have a problem: when you changed something, you have move your cursor over the screen
;; and the git-gutter will refresh its state, else, it won't fucking do anything. of course, this is the god damn linum-mode's problem.
;; (global-linum-mode 1)
;; (setq linum-format " %d ")

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package nyan-mode
  :ensure t
  :config (nyan-mode 1))

(require 'zone)
(zone-when-idle 600)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(require 'posframe)

(if (file-exists-p "~/AppData/Roaming/lsp-bridge")
    (add-to-list 'load-path "~/AppData/Roaming/lsp-bridge")
  (add-to-list 'load-path "~/lsp-bridge"))
(require 'lsp-bridge)
(global-lsp-bridge-mode)

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

(use-package scss-mode
  :ensure t
  :config
  (setq css-indent-offset 2)
  (add-hook 'css-mode-hook
            '(lambda()
               (setq tab-width 4))))

;; (use-package vue-mode
  ;; :ensure t)

;; (use-package tide
;;   :ensure t)
;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (company-mode +1))
;; ;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)
;; ;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)
;; ;; if you use typescript-mode
;; (add-hook 'typescript-mode-hook #'setup-tide-mode)
;; ;; if you use treesitter based typescript-ts-mode (emacs 29+)
;; ;; (add-hook 'typescript-ts-mode-hook #'setup-tide-mode)
;; (add-hook 'javascript-mode-hook #'setup-tide-mode)


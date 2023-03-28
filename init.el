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



;; (require 'evil-escape)
;; (evil-escape-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 150 :family "Source Code Pro")))))

(fringe-mode 0)
(global-linum-mode 1)
(setq linum-format " %d ")
(global-hl-line-mode 1)
(setq ring-bell-function 'ignore)

(set-background-color "#F0FFF2")
(fset 'yes-or-no-p 'y-or-n-p)

(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

(use-package which-key
  :ensure t
  :config (which-key-mode)
  (setq which-key-idle-delay 0.1)
  )

(use-package restart-emacs
  :ensure t)

(use-package better-defaults
  :ensure t)

(use-package counsel
  :ensure t)

(use-package avy
  :ensure t)

(use-package recentf
  :ensure t
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-item 20))

(use-package yasnippet
  :ensure t
  :config
  (add-to-list 'load-path
               "~/.emacs.d/yasnippet")
  (yas-global-mode 1))

(use-package youdao-dictionary
  :ensure t)

(define-key xah-fly-command-map (kbd "C-w s") 'youdao-dictionary-search-at-point-tooltip)

(use-package company
  :ensure t
  :config
  (global-company-mode t)
  (setq company-prefix 1)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  :bind
  (:map company-active-map
        ([tab] . smarter-yas-expand-next-field-complete)
        ("TAB" . smarter-yas-expand-next-field-complete))
  )

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default)
  (setq ac-auto-show-menu 0.02)
  (setq ac-use-menu-map t)
  (define-key ac-menu-map "C-n" 'ac-next)
  (define-key ac-menu-map "C-p" 'ac-previous)
  )

(use-package symbol-overlay
  :ensure t
  :config
  :bind ("M-i" . symbol-overlay-put)
  :bind ("M-n" . symbol-overlay-jump-next)
  :bind ("M-p" . symbol-overlay-jump-prev)
  )
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

(global-set-key (kbd "C-j") 'xah-fly-command-mode-activate)
;; (define-key xah-fly-insert-map (kbd "kj") 'xah-fly-command-mode-activate)













(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auto-complete company youdao-dictionary counsel ivy typescript-mode web-mode vue-mode evil-escape xah-fly-keys)))


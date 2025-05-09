(setq org-directory (file-truename "~/org/"))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (concat org-directory "/roam"))  ; 设置 org-roam 目录
  :after org
  :init
  (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :bind (("C-c n f" . org-roam-node-find)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle))))
  :config
  (org-roam-setup)
  ;;--------------------------
  ;; Handling file properties for ‘LAST_MODIFIED’
  ;;--------------------------
  (defun pv/org-find-time-file-property (property &optional anywhere)
    "Return the position of the time file PROPERTY if it exists.

When ANYWHERE is non-nil, search beyond the preamble."
    (save-excursion
      (goto-char (point-min))
      (let ((first-heading
             (save-excursion
               (re-search-forward org-outline-regexp-bol nil t))))
        (when (re-search-forward (format "^#\\+%s:" property)
                                 (if anywhere nil first-heading)
                                 t)
          (point)))))

  (defun pv/org-has-time-file-property-p (property &optional anywhere)
    "Return the position of time file PROPERTY if it is defined.

As a special case, return -1 if the time file PROPERTY exists but
is not defined."
    (when-let ((pos (pv/org-find-time-file-property property anywhere)))
      (save-excursion
        (goto-char pos)
        (if (and (looking-at-p " ")
                 (progn (forward-char)
                        (org-at-timestamp-p 'lax)))
            pos
          -1))))
  (defun pv/org-set-time-file-property (property &optional anywhere pos)
    "Set the time file PROPERTY in the preamble.

When ANYWHERE is non-nil, search beyond the preamble.

If the position of the file PROPERTY has already been computed,
it can be passed in POS."
    (when-let ((pos (or pos
                        (pv/org-find-time-file-property property))))
      (save-excursion
        (goto-char pos)
        (if (looking-at-p " ")
            (forward-char)
          (insert " "))
        (delete-region (point) (line-end-position))
        (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (insert now)))))

  (defun pv/org-set-last-modified ()
    "Update the LAST_MODIFIED file property in the preamble."
    (when (derived-mode-p 'org-mode)
      (pv/org-set-time-file-property "last_modified")))
  :hook
  ;; 保存文件时调用
  (before-save . pv/org-set-last-modified))


;; 自定义默认模板
;; (setq org-roam-capture-templates
;;    '(("d" "default" plain "%?"
;;       :if-new
;;       (file+head "${slug}-%<%Y%m%d%H%M%S>.org"
;;                  "#+title: ${title}\n#+date: %u\n#+last_modified: \n\n")
;;       :immediate-finish t)))

(provide 'init-org)

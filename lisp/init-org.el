;; Change the asterisk (heading identifier) to some other symbol in Org mode
(use-package org-bullets
             :ensure t
             :config
             (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
             )

;; Org文件以指定的目录深度打开 startup:show2levels
;; #+STARTUP: overview
;; https://emacs-china.org/t/org-startup-show2levels/16499
(setq org-startup-folded 'show-everything)

;; #+STARTUP: indent
;; https://www.wenhui.space/docs/02-emacs/emacs_org_mode/
(setq org-startup-indented t)

;; https://ccdevote.github.io/技术博客/org-mode-basic-4.html
;; #+TODO: TODO(t) SCH(s) WAIT(w) | DONE(d) CANCELLED(c)
(setq org-todo-keywords '((sequence
                            "GATHER(g@)"
                            "NEXT(n@)"
                            "LATER(l@)"
                            "WAIT(w@)"
                            "MAYBE(m@)"
                            "|"
                            "DONE(d@)"
                            "CANCELLED(c@)"
                            "ARCHIVE(a@)"
                            )))

;; 几个常用按键映射
(global-set-key (kbd "C-c l") #'org-store-link)
;; (global-set-key (kbd "C-c C-l") #'org-insert-link) ; 默认的
(global-set-key (kbd "C-c a") #'org-agenda)

;; org-roam
(use-package org-roam
             :ensure t
             :custom
             (org-roam-directory (file-truename "~/depei/repos/org/"))
             :config
             (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:30}" 'face 'org-tag)))
             (setq org-roam-capture-templates
                   '(("d" "default" plain "%?"
                      ;; :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
                      :target (file+head
                                "${slug}.org"
                                "#+title: ${title}
                                #+created: %<%Y/%m/%d %H:%M:%S>"
                                )
                      :unnarrowed t)))
             (org-roam-db-autosync-mode)
             (require 'org-roam-protocol))

;; org-roam-ui
(use-package org-roam-ui
             :ensure t
             :after org-roam
             :config
             (setq org-roam-ui-sync-theme t
                   org-roam-ui-follow t
                   org-roam-ui-update-on-save t
                   org-roam-ui-open-on-start t))

;; 不用evil时的按键映射
(if (not use-evil)
  (progn
    (global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
    (global-set-key (kbd "C-c n f") 'org-roam-node-find)
    (global-set-key (kbd "C-c n i") 'org-roam-node-insert)
    )
  )

;; 把日志记录在LOGBOOK里
(setq org-log-into-drawer t)

;;; agenda默认按列展示
(setq org-agenda-view-columns-initially t)
;; https://orgmode.org/manual/Column-attributes.html
;; (setq org-columns-default-format-for-agenda "%TODO %SCHEDULED %40ITEM %TAGS %1PRIORITY %CATEGORY")
;; (setq org-columns-default-format-for-agenda "%TODO %ALLTAGS %BLOCKED %CATEGORY %CLOCKSUM %CLOCKSUM_T %CLOSED %DEADLINE %FILE %PRIORITY %SCHEDULED %TAGS %TIMESTAMP %TIMESTAMP_IA %ITEM")
;; (setq org-columns-default-format-for-agenda "%TODO %ALLTAGS %BLOCKED %CLOCKSUM %CLOCKSUM_T %CLOSED %DEADLINE %PRIORITY %SCHEDULED %TAGS %TIMESTAMP %TIMESTAMP_IA %ITEM")
(setq org-columns-default-format-for-agenda "%TODO %CLOCKSUM %CLOCKSUM_T %SCHEDULED %TIMESTAMP %TIMESTAMP_IA %ITEM")

;; https://ccdevote.github.io/技术博客/org-mode-basic-4.html
;; #+STARTUP: logdone
(setq org-log-done 'time)

;; 关于org-agenda-files不能奏效的问题
;; https://emacs-china.org/t/org-agenda-files/25932
(setq org-agenda-files (list "~/depei/repos/org/"))

;; 快速打开init.org
(if (not use-evil)
  (global-set-key (kbd "C-x C-<return>") 'open-init-org)
  )

(defun org-add-custom-id-to-heading ()
  "Add a CUSTOM_ID property to the current heading."
  (interactive)
  (let ((custom-id (org-id-new)))
    (org-entry-put nil "CUSTOM_ID" custom-id)
    (message "Added CUSTOM_ID: %s" custom-id)))

;; orgmode的表格对齐
(use-package valign
             :ensure t
             :config
             (setq valign-fancy-bar t)
             (add-hook 'org-mode-hook #'valign-mode))

(provide 'init-org)

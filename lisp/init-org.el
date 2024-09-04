;; Change the asterisk (heading identifier) to some other symbol in Org mode
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

;; Org文件以指定的目录深度打开 startup:show2levels
;; #+STARTUP: overview
;; https://emacs-china.org/t/org-startup-show2levels/16499
;; (setq org-startup-folded 'show-everything)
(setq org-startup-folded 'content)

;; 启动时就生效图片
(setq org-startup-with-inline-images t)

;; #+STARTUP: indent
;; https://www.wenhui.space/docs/02-emacs/emacs_org_mode/
(setq org-startup-indented t)

;; https://ccdevote.github.io/技术博客/org-mode-basic-4.html
;; #+TODO: TODO(t) SCH(s) WAIT(w) | DONE(d) CANCELLED(c)
(setq org-todo-keywords '((sequence
                           "NEXT(n)"
                           "LATER(l)"
                           "WAIT(w@)"
                           "MAYBE(m@)"
                           "|"
                           "DONE(d)"
                           "CANC(c)"
                           )))

;; 几个常用按键映射
(global-set-key (kbd "C-c l") #'org-store-link)
;; (global-set-key (kbd "C-c C-l") #'org-insert-link) ; 默认的
(global-set-key (kbd "C-c a") #'org-agenda)

;; org-roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/depei/repos/org/"))
  (org-roam-dailies-directory (file-truename "~/depei/repos/org/journal"))
  (org-roam-complete-everywhere t)
  :config
  (setq org-roam-file-exclude-regexp '("journal" "data"))
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:30}" 'face 'org-tag)))
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           ;; :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
           :target (file+head
                    "%<%Y%m%d-%H%M%S>.org"
                    "#+title: ${title}"
                    )
           :unnarrowed t)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-dailies)
  (require 'org-roam-protocol))

;; pdf-tools
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode t)))
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;; org-roam-ui
(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

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

;; orgmode的表格对齐
(use-package valign
  :ensure t
  :config
  (setq valign-fancy-bar t)
  (add-hook 'org-mode-hook #'valign-mode))

;; org插入图片
(use-package org-download
  :ensure t
  :config
  (setq-default org-download-heading-lvl nil)
  (setq-default org-download-image-dir "~/depei/repos/org/data/images")
  (setq org-download-abbreviate-filename-function (lambda (fn) fn)) ; use original filename
  (defun dummy-org-download-annotate-function (link) "")
  (setq org-download-annotate-function #'dummy-org-download-annotate-function)
  (setq org-download-screenshot-method "powershell -c Add-Type -AssemblyName System.Windows.Forms;$image = [Windows.Forms.Clipboard]::GetImage();$image.Save('%s', [System.Drawing.Imaging.ImageFormat]::Png)")
  (setq org-download-abbreviate-filename-function 'abbreviate-file-name)
  )

;; org roam高级接口
(use-package consult-org-roam
  :ensure t)

(provide 'init-org)

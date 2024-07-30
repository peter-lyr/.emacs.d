; TODO: 需要先创建~/org/roam目录

(setq org-directory (file-truename "~/org/"))

(use-package org-roam
   :ensure t
   :after org
   :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
   :config (org-roam-setup)
   :custom (org-roam-directory (concat org-directory "roam/")) ; 设置 org-roam 目录
   :bind (("C-c n f" . org-roam-node-find)
          ("C-c n i" . org-roam-node-insert)
          ("C-c n o" . org-id-get-create)
          ("C-c n t" . org-roam-tag-add)
          ("C-c n a" . org-roam-alias-add)
          ("C-c n l" . org-roam-buffer-toggle)))

(provide 'init-org)

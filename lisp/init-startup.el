;; 关闭Emacs的欢迎界面
(setq inhibit-startup-screen t)

;; 无工具栏，菜单栏和滚动栏
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; 主题还是要有的
(use-package dracula-theme
  :ensure t
  :config (load-theme 'dracula t))

;; (use-package monokai-theme
;;              :ensure t
;;              :config (load-theme 'monokai t))

;; (load-theme 'tango-dark t)

;; 无边框
(set-frame-parameter nil 'undecorated t)

;; 设置字体
(set-face-attribute 'default nil :font "Hack NFM")

;; ;; 背景透明
;; (set-frame-parameter nil 'alpha '(100 . 78))

;; 让emacs启动就全屏
;; (add-hook 'window-setup-hook #'toggle-frame-maximized t)
;; 50行160列
(set-frame-height (selected-frame) 50)
(set-frame-width (selected-frame) 160)

;; 显示行号
(global-display-line-numbers-mode 1)

;; 当另一程序修改了文件时，让Emacs及时刷新Buffer
(global-auto-revert-mode t)

;; 自动保存
;; http://xahlee.info/emacs/emacs/emacs_auto_save.html
;; Emacs: Real Automatic Save File
(auto-save-visited-mode 1)
(setq auto-save-visited-interval 30)
;; Auto Save File When Switching Out of Emacs
(defun save-all-unsaved ()
  (interactive)
  (save-some-buffers t))
(setq after-focus-change-function 'save-all-unsaved)

;; Emacs禁止自动产生备份文件
(setq make-backup-files nil)

;; 编码系统
;; 解决每次退出都要提示一次，要按好多次才能退出的问题
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)

;; ;; 报错日志
;; (setq debug-on-error t)

;; Open org link in the same window
(setq org-link-frame-setup
      '((vm . vm-visit-folder-other-frame)
        (vm-imap . vm-visit-imap-folder-other-frame)
        (gnus . org-gnus-no-new-news)
        (file . find-file)
        (wl . wl-other-frame)))

;; emacs重启
;; https://github.com/iqbalansari/restart-emacs
(use-package restart-emacs
  :ensure t)

;; emacs重启
(defun my-restart-emacs()
  (interactive)
  (restart-emacs-start-new-emacs)
  (save-buffers-kill-terminal))
(global-set-key (kbd "C-x C-<delete>") 'my-restart-emacs) ;; 以防有时evil死掉了

;; 自动补全括号
(electric-pair-mode t)

;; 编程模式下，光标在括号上时高亮另一个括号
(add-hook 'prog-mode-hook #'show-paren-mode)

;; 历史文件
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;; 不要使用制表符
(setq-default indent-tabs-mode nil)

(defun my-write (log-message log-file)
  "Add a given message string to the end of a file."
  (interactive)
  (write-region log-message nil log-file))

;; 失去emacs焦点时写个管道参数给mouse
(add-hook 'focus-out-hook
          (lambda ()
            (if (eq evil-state 'insert)
              (my-write "1" "C:\\Windows\\Temp\\emacs.exe-input-method.txt")
              (my-write "0" "C:\\Windows\\Temp\\emacs.exe-input-method.txt")
              )
            ))

(provide 'init-startup)

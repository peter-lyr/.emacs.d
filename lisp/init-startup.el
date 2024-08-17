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
  (save-some-buffers t ))
(setq after-focus-change-function 'save-all-unsaved)

;; Emacs禁止自动产生备份文件
(setq make-backup-files nil)

;; 编码系统
;; 解决每次退出都要提示一次，要按好多次才能退出的问题
(set-default-coding-systems 'utf-8)

;; 报错日志
(setq debug-on-error t)

(provide 'init-startup)

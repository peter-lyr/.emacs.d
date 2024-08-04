(require 'package)
(add-to-list 'package-archives
             '("gnu" . "https://mirrors.163.com/elpa/gnu/")
             '("melpa" . "https://mirrors.163.com/elpa/melpa/")
             )
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

; vi模式
(use-package evil
             :ensure t
             :config (evil-mode 1))

; 主题
(use-package dracula-theme
             :ensure t
             :config (load-theme 'dracula t))

; 无工具栏，菜单栏和滚动栏
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

; 无边框
(set-frame-parameter nil 'undecorated t)

; ; 背景透明
; (set-frame-parameter nil 'alpha '(100 . 78))

; 让emacs启动就全屏
(add-hook 'window-setup-hook #'toggle-frame-maximized t)

; 关闭Emacs的欢迎界面
(custom-set-variables
  '(inhibit-startup-screen t))

; 显示行号
(global-display-line-numbers-mode 1)

; 当另一程序修改了文件时，让Emacs及时刷新Buffer
(global-auto-revert-mode t)

; Org文件以指定的目录深度打开 startup:show2levels
; #+STARTUP: overview
; https://emacs-china.org/t/org-startup-show2levels/16499
(setq org-startup-folded 'show2levels)

; #+STARTUP: indent
; https://www.wenhui.space/docs/02-emacs/emacs_org_mode/
(setq org-startup-indented t)

; https://ccdevote.github.io/技术博客/org-mode-basic-4.html
; #+TODO: TODO(t) SCH(s) WAIT(w) | DONE(d) CANCELLED(c)
(setq org-todo-keywords
      '((sequence "TODO" "SCH" "WAIT" "|" "DONE" "CANCELLED")))

; https://ccdevote.github.io/技术博客/org-mode-basic-4.html
; #+STARTUP: logdone
(setq org-log-done 'time)

; 上电打开org文件
(find-file "~/depei/repos/org/test.org")

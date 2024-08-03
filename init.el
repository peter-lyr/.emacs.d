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

; 背景透明
(set-frame-parameter nil 'alpha '(85 . 50))

; 让emacs启动就全屏
(add-hook 'window-setup-hook #'toggle-frame-maximized t)

; 关闭Emacs的欢迎界面
(custom-set-variables
  '(inhibit-startup-screen t))

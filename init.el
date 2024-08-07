(require 'package)
(add-to-list 'package-archives
             '("gnu" . "https://mirrors.163.com/elpa/gnu/")
             '("melpa" . "https://mirrors.163.com/elpa/melpa/")
             )
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

; 是否使能vi模式
(if 1
  ; vi模式
  (use-package evil
               :ensure t
               :config (evil-mode 1))
  ; emacs 改变光标形状
  ; http://blog.chinaunix.net/uid-20609878-id-1915848.html
  (setq-default cursor-type 'bar))

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

; 关于org-agenda-files不能奏效的问题
; https://emacs-china.org/t/org-agenda-files/25932
(setq org-agenda-files (list "~/depei/repos/org/"))

; M-RET不要切割当前行
(setq org-M-RET-may-split-line nil)

; 几个常用按键映射
; (global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
; (global-set-key (kbd "C-c c") #'org-capture)

; 上电打开org文件
; https://superuser.com/questions/400457/how-to-automatically-open-a-file-when-emacs-start
(find-file "~/depei/repos/org/test.org")

; http://xahlee.info/emacs/emacs/emacs_auto_save.html
; Emacs: Real Automatic Save File
(auto-save-visited-mode 1)
(setq auto-save-visited-interval 30)
; Auto Save File When Switching Out of Emacs
(defun xah-save-all-unsaved ()
  (interactive)
  (save-some-buffers t ))
(setq after-focus-change-function 'xah-save-all-unsaved)

; https://emacs-china.org/t/org-mode/2195/4
; C-u C-c . 当前时间
; C-c .00:00 指定时间为 00:00
; C-c .+1d 当前日期加 1

; C-u C-c . vs C-c .
; https://emacs.stackexchange.com/questions/71304/what-is-the-difference-between-c-u-c-c-c-l-and-c-c-c-l
; https://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Command-Arguments.html

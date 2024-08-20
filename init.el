(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; 定义require的寻找路径
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))

;; 自定义配置路径，不改则保存到次文件
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))

;; 是否使能vi模式，后面会用到
(setq use-evil t)

;; 是否使which-key
(setq use-which-key t)

(if use-evil
    (progn
      ;; 开启vi模式
      (use-package evil
        :ensure t
        :init
        ;; (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
        (setq evil-want-keybinding nil)
        :config
        (evil-mode 1))
      (use-package evil-leader
        :ensure t
        :config
        (global-evil-leader-mode)
        (evil-leader/set-leader "SPC"))
      (use-package evil-org
        :ensure t
        :after org
        :hook (org-mode . evil-org-mode)
        :config
        (evil-org-set-key-theme)
        (require 'evil-org-agenda)
        ;; (evil-org-agenda-set-keys) ;; See https://github.com/Somelauw/evil-org-mode/blob/master/evil-org-agenda.el
        (evil-set-initial-state 'org-agenda-mode 'motion)
        (evil-define-key 'motion org-agenda-mode-map
          "'" 'org-agenda-show-and-scroll-up
          "j" 'org-agenda-next-line
          "k" 'org-agenda-previous-line
          "gw" 'org-agenda-week-view
          "gm" 'org-agenda-month-view
          "gy" 'org-agenda-year-view
          "g." 'org-agenda-toggle-time-grid
          "gt" 'org-agenda-goto-today
          "gl" 'org-agenda-log-mode
          "f" 'org-agenda-later
          "b" 'org-agenda-earlier
          "n" 'org-agenda-add-note
          )
        )
      (use-package evil-collection
        :ensure t
        :after evil
        :config
        (evil-collection-init))
      )
  ;; 不用evil
  ;; emacs 改变光标形状
  ;; http://blog.chinaunix.net/uid-20609878-id-1915848.html
  (setq-default cursor-type 'bar))

(require 'init-funcs) ; 所有定义的函数放这里
(require 'init-org) ; orgmode配置
(require 'init-startup) ; 一些系统级配置
(require 'init-test) ; 测试或易忘的配置

;; avy查找并将光标位置快速跳到某个字符
(if use-evil
    (use-package avy
      :ensure t)
  (use-package avy
    :ensure t
    :bind
    (("C-c j" . avy-goto-char))
    )
  )

;; 窗口最大化
(use-package zoom-window
  :ensure t
  :config
  (setq zoom-window-mode-line-color nil)
  )

;; helm查找文件
(use-package helm :ensure t)

;; 代码格式化
(use-package format-all
  :ensure t
  :config
  (global-set-key (kbd "M-F") #'ian/format-code)
  (add-hook 'prog-mode-hook #'format-all-ensure-formatter)
  )

(if use-which-key
    ;; which-key
    (use-package which-key
      :ensure t
      :config
      (setq which-key-popup-type 'minibuffer)
      (which-key-mode)
      )
  )

(if use-which-key
    ;; which-key的prefix定义
    (progn
      (which-key-add-key-based-replacements "SPC f" "find-file/frame/format")
      (which-key-add-key-based-replacements "SPC f e" "find-file open config el")
      (which-key-add-key-based-replacements "SPC f o" "find-file open org")
      (which-key-add-key-based-replacements "SPC f r" "frame")
      (which-key-add-key-based-replacements "SPC w" "window")
      (which-key-add-key-based-replacements "SPC n" "note")
      (which-key-add-key-based-replacements "SPC b" "buffer")
      )
  )

(defun evil-leader-set-key ()
  (interactive)
  ;; 打开init.el文件
  (evil-leader/set-key "fei" 'open-init-el)
  (evil-leader/set-key "feo" 'open-init-org-el)
  ;; 查找文件
  (evil-leader/set-key "SPC" 'helm-find-files)
  ;; 复制文件路径
  (evil-leader/set-key "fy" 'copy-buffer-file-name)
  ;; avy
  (evil-leader/set-key "s" 'avy-goto-char)
  ;; dired
  (evil-leader/set-key "fj" 'dired-jump)
  ;; 打开org文件
  (evil-leader/set-key "foi" 'open-init-org)
  (evil-leader/set-key "fow" 'open-work-org)
  ;; org agenda
  (evil-leader/set-key-for-mode 'org-mode "no" 'org-agenda)
  (evil-leader/set-key-for-mode 'org-mode "na" 'org-agenda-list)
  (evil-leader/set-key-for-mode 'org-mode "nt" 'org-todo-list)
  ;; org roam
  (evil-leader/set-key-for-mode 'org-mode "nl" 'org-roam-buffer-toggle)
  (evil-leader/set-key-for-mode 'org-mode "nf" 'org-roam-node-find)
  (evil-leader/set-key-for-mode 'org-mode "ni" 'org-roam-node-insert)
  ;; org roam ui
  (evil-leader/set-key-for-mode 'org-mode "nu" 'org-roam-ui-mode)
  ;; 窗口复制
  (evil-leader/set-key "ws" 'evil-window-split)
  (evil-leader/set-key "wv" 'evil-window-vsplit)
  ;; 窗口跳转
  (evil-leader/set-key "wl" 'evil-window-right)
  (evil-leader/set-key "wh" 'evil-window-left)
  (evil-leader/set-key "wk" 'evil-window-up)
  (evil-leader/set-key "wj" 'evil-window-down)
  ;; 窗口关闭
  (evil-leader/set-key "wd" 'evil-quit)
  ;; 下一个窗口/切换
  (evil-leader/set-key "wp" 'evil-window-mru)
  (evil-leader/set-key "wn" 'evil-window-next)
  ;; 上/下一个buffer
  (evil-leader/set-key "bn" 'next-buffer)
  (evil-leader/set-key "bp" 'previous-buffer)
  (evil-leader/set-key "bb" 'list-buffers)
  ;; 关闭buffer
  (evil-leader/set-key "bd" 'evil-delete-buffer)
  ;; emacs关闭或重启
  (evil-leader/set-key "frr" 'my-restart-emacs)
  (evil-leader/set-key "frq" 'save-buffers-kill-terminal)
  ;; 重新刷新package-archive-contents
  (evil-leader/set-key "frp" 'refresh-package-archive-contents)
  ;; 窗口最大化和一样大
  (evil-leader/set-key "wm" 'zoom-window-zoom)
  (evil-leader/set-key "we" 'balance-windows)
  ;; 回车键怎么能少得了呢
  ;; spacemacs\layers\+spacemacs\spacemacs-org\packages.el #61
  (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)
  ;; 一次跳5行
  (define-key evil-motion-state-map (kbd "C-j") 'next-5-lines)
  (define-key evil-motion-state-map (kbd "C-k") 'prev-5-lines)
  ;; 代码格式化
  (evil-leader/set-key "ff" 'format-code)
  )

;; 统一在这里设置按键映射
(if use-evil
    (evil-leader-set-key)
  )

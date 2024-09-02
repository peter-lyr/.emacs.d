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

;; 是否使which-key
(setq use-which-key t)

;; ================================================================
;; evil插件
;; ================================================================

;; 开启vi模式
(use-package evil
  :ensure t
  :init
  ;; (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  ;; 切换输入法
  (add-hook 'evil-insert-state-exit-hook 'exit-insert-mode-hook)
  (add-hook 'evil-insert-state-entry-hook 'enter-insert-mode-hook)
  )

;; vi的leader
(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "SPC"))

;; 解决agenda按键不能vi的问题
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
    ",w" 'org-agenda-week-view
    ",m" 'org-agenda-month-view
    ",y" 'org-agenda-year-view
    ",." 'org-agenda-toggle-time-grid
    ",t" 'org-agenda-goto-today
    ",l" 'org-agenda-log-mode
    "f" 'org-agenda-later
    "b" 'org-agenda-earlier
    "n" 'org-agenda-add-note
    )
  )

;; 解决agenda按键不能vi的问题
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(require 'init-funcs) ; 所有定义的函数放这里
(require 'init-org) ; orgmode配置
(require 'init-startup) ; 一些系统级配置
(require 'init-test) ; 测试或易忘的配置

;; ================================================================
;; 其他插件
;; ================================================================

;; avy查找并将光标位置快速跳到某个字符
(use-package avy :ensure t)

;; 窗口最大化
(use-package zoom-window
  :ensure t
  :config
  (setq zoom-window-mode-line-color nil)
  )

;; ivy查找文件（开始）
(use-package ivy
  :ensure t
  :demand t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 35)
  )
(use-package counsel
  :ensure t
  :after ivy
  :config
  (counsel-mode 1)
  )
(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode)
  )

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
      (which-key-add-key-based-replacements "SPC w" "window")
      (which-key-add-key-based-replacements "SPC x" "window close")
      (which-key-add-key-based-replacements "SPC w s" "window split")
      (which-key-add-key-based-replacements "SPC n" "note(org)")
      (which-key-add-key-based-replacements "SPC n r" "note roam")
      (which-key-add-key-based-replacements "SPC n l" "note link")
      (which-key-add-key-based-replacements "SPC n h" "note heading")
      (which-key-add-key-based-replacements "SPC n m" "note my")
      (which-key-add-key-based-replacements "SPC n i" "note image")
      (which-key-add-key-based-replacements "SPC n j" "note journal")
      (which-key-add-key-based-replacements "SPC n s" "note timestamp time")
      (which-key-add-key-based-replacements "SPC n s d" "note timestamp date")
      (which-key-add-key-based-replacements "SPC b" "buffer")
      )
  )

;; ================================================================
;; 按键映射
;; ================================================================

;; 统一在这里设置按键映射
;; 打开init.el文件
(evil-leader/set-key "fei" 'open-init-el)
(evil-leader/set-key "feo" 'open-init-org-el)
;; 查找文件
(evil-leader/set-key "SPC" 'counsel-projectile-find-file)
;; 复制文件路径
(evil-leader/set-key "fy" 'copy-buffer-file-name)
;; avy
(evil-leader/set-key "s" 'avy-goto-char)
;; (evil-define-key 'normal org-mode-map (kbd "s") 'avy-goto-char)
(evil-define-key 'motion 'global (kbd "s") 'avy-goto-char)
;; dired
(evil-leader/set-key "fj" 'dired-jump)
;; 打开recentf
(evil-leader/set-key "fo" 'counsel-recentf)
;; 打开org文件
(evil-leader/set-key "fi" 'open-init-org)
(evil-leader/set-key "fw" 'open-work-org)
;; org journal
(evil-leader/set-key "n j SPC" 'org-roam-dailies-goto-today)
(evil-leader/set-key "n j h" 'org-roam-dailies-goto-yestoday)
(evil-leader/set-key "n j l" 'org-roam-dailies-goto-tomorrow)
(evil-leader/set-key "n j j" 'org-roam-dailies-find-directory)
(evil-leader/set-key "n j RET" 'org-roam-dailies-capture-today)
(evil-leader/set-key "n j c SPC" 'org-roam-dailies-capture-today)
(evil-leader/set-key "n j c h" 'org-roam-dailies-capture-yestoday)
(evil-leader/set-key "n j c l" 'org-roam-dailies-capture-tomorrow)
;; org image
(evil-leader/set-key-for-mode 'org-mode "n i SPC" 'org-toggle-inline-images)
(evil-leader/set-key-for-mode 'org-mode "nii" 'org-download-screenshot)
;; org time stamp
(evil-leader/set-key-for-mode 'org-mode "nsi" 'my-org-time-stamp-inactive)
(evil-leader/set-key-for-mode 'org-mode "nsa" 'my-org-time-stamp)
(evil-leader/set-key-for-mode 'org-mode "nsdi" 'org-time-stamp-inactive)
(evil-leader/set-key-for-mode 'org-mode "nsda" 'org-time-stamp)
;; org store/insert link
(evil-leader/set-key-for-mode 'org-mode "n l SPC" 'org-toggle-link-display)
(evil-leader/set-key-for-mode 'org-mode "nls" 'org-store-link)
(evil-leader/set-key-for-mode 'org-mode "nli" 'org-insert-link)
;; org find headings
(evil-leader/set-key-for-mode 'org-mode "nhh" 'counsel-outline)
(evil-leader/set-key-for-mode 'org-mode "nha" 'counsel-org-agenda-headlines)
;; org agenda
(evil-leader/set-key-for-mode 'org-mode "no" 'org-agenda)
(evil-leader/set-key-for-mode 'org-mode "na" 'org-agenda-list)
(evil-leader/set-key-for-mode 'org-mode "nt" 'org-todo-list)
;; org roam
(evil-leader/set-key-for-mode 'org-mode "nrl" 'my-org-roam-buffer-toggle)
(evil-leader/set-key-for-mode 'org-mode "nrn" 'org-roam-node-find)
(evil-leader/set-key-for-mode 'org-mode "nri" 'org-roam-node-insert)
;; org roam consult
(evil-leader/set-key-for-mode 'org-mode "nb" 'consult-org-roam-backlinks)
(evil-leader/set-key-for-mode 'org-mode "nf" 'consult-org-roam-forward-links)
;; org my
(evil-leader/set-key-for-mode 'org-mode "nmc" 'org-add-custom-id-to-heading)
;; org roam ui
(evil-leader/set-key-for-mode 'org-mode "nu" 'org-roam-ui-mode)
;; 窗口复制
(evil-leader/set-key "wsl" 'window-split-right)
(evil-leader/set-key "wsh" 'window-split-left)
(evil-leader/set-key "wsk" 'window-split-up)
(evil-leader/set-key "wsj" 'window-split-down)
;; 窗口跳转
(evil-leader/set-key "wl" 'evil-window-right)
(evil-leader/set-key "wh" 'evil-window-left)
(evil-leader/set-key "wk" 'evil-window-up)
(evil-leader/set-key "wj" 'evil-window-down)
;; 窗口关闭
(evil-leader/set-key "wd" 'evil-quit)
(evil-leader/set-key "xl" 'window-close-right)
(evil-leader/set-key "xh" 'window-close-left)
(evil-leader/set-key "xk" 'window-close-up)
(evil-leader/set-key "xj" 'window-close-down)
;; 下一个窗口/切换
(evil-leader/set-key "wp" 'evil-window-mru)
(evil-leader/set-key "wn" 'evil-window-next)
;; 上/下一个buffer
(evil-leader/set-key "bn" 'next-buffer)
(evil-leader/set-key "bp" 'previous-buffer)
(evil-leader/set-key "bb" 'go-all-buffers)
;; message buffer
(evil-leader/set-key "bm" 'go-buffer-messages)
(evil-leader/set-key "bs" 'go-buffer-scratch)
;; 关闭buffer
(evil-leader/set-key "bd" 'evil-delete-buffer)
;; emacs关闭或重启
(evil-leader/set-key "frr" 'my-restart-emacs)
(evil-leader/set-key "frq" 'save-all-unsaved-and-quit)
;; 重新刷新package-archive-contents
(evil-leader/set-key "frp" 'refresh-package-archive-contents)
;; 窗口最大化和一样大
(evil-leader/set-key "wz" 'zoom-window-zoom)
(evil-leader/set-key "wm" 'evil-window-set-height)
(evil-leader/set-key "w," 'evil-window-set-width)
(evil-leader/set-key "we" 'balance-windows)
;; 代码格式化
(evil-leader/set-key "ff" 'my-format-code)
(evil-define-key 'normal org-mode-map (kbd "SPC f f") 'my-format-code)
;; 回车键怎么能少得了呢
;; spacemacs\layers\+spacemacs\spacemacs-org\packages.el #61
(evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)
;; 一次跳5行
(evil-define-key '(normal insert) org-mode-map (kbd "C-j") 'next-5-lines)
(evil-define-key '(normal insert) org-mode-map (kbd "C-k") 'prev-5-lines)
(define-key evil-motion-state-map (kbd "C-j") 'next-5-lines)
(define-key evil-motion-state-map (kbd "C-k") 'prev-5-lines)
;; zj/k
(evil-define-key 'normal org-mode-map (kbd "zj") 'org-next-visible-heading)
(evil-define-key 'normal org-mode-map (kbd "zk") 'org-previous-visible-heading)

;; ================================================================
;; 其他设置
;; ================================================================

;; 保存历史记录
(add-to-list 'savehist-additional-variables 'search-ring)
(add-to-list 'savehist-additional-variables 'regexp-search-ring)
(add-to-list 'savehist-additional-variables 'kill-ring)
;; (setq savehist-autosave-interval 500) ;; 默认 500，需要在 (savehist-mode t) 之前设置，否则无效
(savehist-mode t)

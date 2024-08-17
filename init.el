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
                 :hook (org-mode . 'evil-org-mode)
                 :config
                 (evil-org-set-key-theme)
                 (require 'evil-org-agenda)
                 (evil-org-agenda-set-keys)
                 )
    (use-package evil-collection
                 :after evil
                 :config
                 (evil-collection-init))
    )
  ;; 不用evil
  ;; emacs 改变光标形状
  ;; http://blog.chinaunix.net/uid-20609878-id-1915848.html
  (setq-default cursor-type 'bar))

(require 'init-org) ; orgmode配置
(require 'init-startup) ; 一些系统级配置
(require 'init-test) ; 测试或易忘的配置

;; avy查找并将光标位置快速跳到某个字符
(if use-evil
  (use-package avy)
  (use-package avy :bind (("C-c j" . avy-goto-char)))
  )

(defun evil-leader-set-key ()
  (interactive)
  ;; avy
  (evil-leader/set-key "f;" 'avy-goto-char)
  ;; dired
  (evil-leader/set-key "fj" 'dired-jump)
  ;; 快速打开org文件
  (evil-leader/set-key "fo" 'open-init-org)
  ;; org roam
  (evil-leader/set-key "nl" 'org-roam-buffer-toggle)
  (evil-leader/set-key "nf" 'org-roam-node-find)
  (evil-leader/set-key "ni" 'org-roam-node-insert)
  ;; 窗口复制
  (evil-leader/set-key "ws" 'evil-window-split)
  (evil-leader/set-key "wv" 'evil-window-vsplit)
  ;; 窗口跳转
  (evil-leader/set-key "wl" 'evil-window-right)
  (evil-leader/set-key "wh" 'evil-window-left)
  (evil-leader/set-key "wk" 'evil-window-up)
  (evil-leader/set-key "wj" 'evil-window-down)
  (evil-leader/set-key "wd" 'evil-quit)
  ;; 回车键怎么能少得了呢
  ;; spacemacs\layers\+spacemacs\spacemacs-org\packages.el #61
  (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)
  )

;; 统一在这里设置按键映射
(if use-evil
  (evil-leader-set-key)
  )

(require 'package)
(add-to-list 'package-archives
        '("gnu" . "https://mirrors.163.com/elpa/gnu/")
        '("melpa" . "https://mirrors.163.com/elpa/melpa/")
        )

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(use-package evil
  :ensure t
  :config (evil-mode 1))

(use-package dracula-theme
  :ensure t
  :config (load-theme 'dracula t))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-frame-parameter nil 'undecorated t)

(custom-set-variables
 '(inhibit-startup-screen t))

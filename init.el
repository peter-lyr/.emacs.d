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

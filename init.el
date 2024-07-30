(setq package-archives
      '(

        ("gnu" . "https://mirrors.163.com/elpa/gnu/")
        ("melpa" . "https://mirrors.163.com/elpa/melpa/")
        ("melpa-stable" . "https://mirrors.163.com/elpa/stable-melpa/")

        ; ("gnu" . "https://elpa.gnu.org/packages/")
        ; ("melpa" . "https://melpa.org/packages/")
        ; ("melpa-stable" . "https://stable.melpa.org/packages/")

        ))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-startup)
(require 'init-theme)
(require 'init-evil)
(require 'init-org)

; TODO: If not exists, it's warning
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

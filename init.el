(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-startup)
(require 'init-theme)
(require 'init-evil)

; TODO: If not exists, it's warning
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

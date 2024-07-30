(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-startup)
(require 'init-theme)
(require 'init-evil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

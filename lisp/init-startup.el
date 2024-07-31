(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(custom-set-variables
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil))

(global-display-line-numbers-mode 1)

; (make-frame '((undecorated . t))) ; new one frame
(set-frame-parameter nil 'undecorated t)

(provide 'init-startup)

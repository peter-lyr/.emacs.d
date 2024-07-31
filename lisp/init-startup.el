(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(custom-set-variables
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil))

(global-display-line-numbers-mode 1)

; (make-frame '((undecorated . t))) ; new one frame
(set-frame-parameter nil 'undecorated t)

(set-frame-parameter nil 'alpha '(85 . 50))

; (defun toggle-transparency ()
;   (interactive)
;   (let ((alpha (frame-parameter nil 'alpha)))
;     (if (eq
;           (if (numberp alpha)
;             alpha
;             (cdr alpha)) ; may also be nil
;           100)
;       (set-frame-parameter nil 'alpha '(85 . 50))
;       (set-frame-parameter nil 'alpha '(100 . 100))
;       )
;     )
;   )
; (global-set-key (kbd "C-c t") 'toggle-transparency)

; 不要备份
; (setq make-backup-files nil)

(provide 'init-startup)

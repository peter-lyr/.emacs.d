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
(setq make-backup-files nil)

; auto-save-mode好像不行，auto-save-visited-mode可以
; (auto-save-mode)
; (setq auto-save-interval 1)
(auto-save-visited-mode)
; auto-save-visited-interval好像改变不了
; (setq auto-save-visited-interval 1)

; 当另一程序修改了文件时，让Emacs及时刷新Buffer
(global-auto-revert-mode t)

; 在Mode line上显示列号
(column-number-mode t)

; 自动补全括号
(electric-pair-mode t)

; 编程模式下，光标在括号上时高亮另一个括号
(add-hook 'prog-mode-hook #'show-paren-mode)

(provide 'init-startup)

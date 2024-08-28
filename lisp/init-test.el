;; https://emacs-china.org/t/org-mode/2195/4
;; C-u C-c . 当前时间
;; C-c .00:00 指定时间为 00:00
;; C-c .+1d 当前日期加 1

;; C-u C-c . vs C-c .
;; https://emacs.stackexchange.com/questions/71304/what-is-the-difference-between-c-u-c-c-c-l-and-c-c-c-l
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Command-Arguments.html

;; ;; M-RET不要切割当前行
;; (setq org-M-RET-may-split-line nil)

;; 调用后有好多按键映射会失灵
;; (defun reload-init-el ()
;;   (interactive)
;;   (load-file "~/.emacs.d/init.el")
;;   )
;; ;; 重新加载init.el
;; (evil-leader/set-key "li" 'reload-init-el)

;; normal模式
;; (define-key evil-normal-state-map (kbd "C-j") 'next-5-lines)
;; (define-key evil-normal-state-map (kbd "C-k") 'prev-5-lines)

;; ■  Error (use-package): Failed to install helm: https://melpa.org/packages/helm-core-20240813.1920.tar: Not found
;; ■  Error (use-package): Cannot load helm

;; (defun remove-trailing-whitespace ()
;;   "Remove trailing whitespace in the current buffer."
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (re-search-forward "[ \t]+$" nil t)
;;       (replace-match "" nil nil))))
;;
;; ;; 绑定到快捷键
;; (global-set-key (kbd "C-c C-w") 'remove-trailing-whitespace)

;; (defun my-test ()
;;   "test"
;;   (interactive)
;;   (switch-to-buffer-window "*Test*")
;;   )

;; (defun my/org-add-ids-to-headlines-in-file ()
;;   "Add CUSTOM_ID properties to all headlines in the current file."
;;   (interactive)
;;   (org-map-entries
;;    (lambda ()
;;      (let ((custom-id (org-entry-get nil "CUSTOM_ID")))
;;        (unless custom-id
;;          (setq custom-id (org-id-new))
;;          (org-entry-put nil "CUSTOM_ID" custom-id))))))
;;
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (my/org-add-ids-to-headlines-in-file)
;;             ))

;; ;; 不用evil
;; ;; emacs 改变光标形状
;; ;; http://blog.chinaunix.net/uid-20609878-id-1915848.html
;; (setq-default cursor-type 'bar))

(provide 'init-test)

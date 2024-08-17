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

(provide 'init-test)

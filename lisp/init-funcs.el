(defun prev-5-lines ()
  "向上移动5行"
  (interactive)
  (evil-previous-line 5)
  )

(defun next-5-lines ()
  "向下移动5行"
  (interactive)
  (evil-next-line 5)
  )

(defun open-init-el ()
  "打开init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )

(defun open-init-org-el ()
  "打开init-org.el"
  (interactive)
  (find-file "~/.emacs.d/lisp/init.el")
  )

(defun open-init-org()
  "上电打开init.org文件"
  (interactive)
  (find-file "~/depei/repos/org/init.org"))

(defun refresh-package-archive-contents ()
  "重新刷新package-archive-contents"
  (interactive)
  (package-refresh-contents)
  )

(defun copy-buffer-file-name ()
  "Copy the full path to the current file and show in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
      (progn
        (message file-name)
        (kill-new file-name))
      (error "Buffer not visiting a file"))))

(provide 'init-funcs)

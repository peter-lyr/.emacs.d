(defun prev-5-lines ()
  "向上移动5行"
  (interactive)
  (evil-previous-line 5)
  )

(defun window-split-up ()
  "split window to up"
  (interactive)
  (evil-window-split)
  )

(defun window-split-down ()
  "split window to down"
  (interactive)
  (evil-window-split)
  (other-window 1)
  )

(defun window-split-right ()
  "split window to right"
  (interactive)
  (evil-window-vsplit)
  (other-window 1)
  )

(defun window-split-left ()
  "split window to left"
  (interactive)
  (evil-window-vsplit)
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
  (find-file "~/.emacs.d/lisp/init-org.el")
  )

(defun open-init-org()
  "上电打开init.org文件"
  (interactive)
  (find-file "~/depei/repos/org/init.org"))

(defun open-work-org()
  "上电打开work.org文件"
  (interactive)
  (find-file "~/depei/repos/org/work.org"))

(defun refresh-package-archive-contents ()
  "重新刷新package-archive-contents"
  (interactive)
  (package-refresh-contents)
  )

(defun copy-buffer-file-name (choice)
  "Copyies the buffer {name/mode}, file {name/full path/directory} to the kill-ring.
  See https://stackoverflow.com/questions/2416655/file-path-to-clipboard-in-emacs"
  (interactive "cCopy (b) buffer name, (m) buffer major mode, (f) full buffer-file path, (d) buffer-file directory, (n) buffer-file basename")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          ((eq choice ?b)
           (setq new-kill-string (buffer-name)))
          ((eq choice ?m)
           (setq new-kill-string (format "%s" major-mode)))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))

(defun format-code ()
  "format whole buffer."
  (interactive)
  (if (derived-mode-p 'prolog-mode)
      (prolog-indent-buffer)
    (format-all-buffer)))

(provide 'init-funcs)

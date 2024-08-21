(setq dot-emacs-dot-d (file-name-directory user-init-file))
(setq lisp-dir (format "%slisp/" dot-emacs-dot-d))
(setq input-method-py (format "%sinput-method.py" lisp-dir))

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

(defun window-close-up ()
  "close up window"
  (interactive)
  (windmove-up)
  (delete-window)
  )

(defun window-close-down ()
  "close down window"
  (interactive)
  (windmove-down)
  (delete-window)
  )

(defun window-close-right ()
  "close right window"
  (interactive)
  (windmove-right)
  (delete-window)
  )

(defun window-close-left ()
  "close left window"
  (interactive)
  (windmove-left)
  (delete-window)
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

(defun switch-input-method (lang)
  "switch input method, LANG: ZH or EN"
  (shell-command (format "python %s %s" input-method-py lang))
  )

(defun switch-input-method-to-zh ()
  "switch input method to ZH"
  (interactive)
  (switch-input-method "ZH")
  )

(defun switch-input-method-to-en ()
  "switch input method to EN"
  (interactive)
  (switch-input-method "EN")
  )

(defun exit-insert-mode-hook ()
  "exit-insert-mode-hook"
  (switch-input-method-to-en)
  )

(defun enter-insert-mode-hook ()
  "enter-insert-mode-hook"
  (switch-input-method-to-zh)
  )

(defun go-buffer-messages ()
  "go *Messages* buffer"
  (interactive)
  (switch-to-buffer "*Messages*")
  )

(defun go-buffer-scratch ()
  "go *scratch* buffer"
  (interactive)
  (switch-to-buffer "*scratch")
  )

;; emacs怎么跳转到指定buffer所在的window
(defun switch-to-buffer-window (buffer-name)
  "Switch to the window displaying BUFFER-NAME."
  (let ((buffer (get-buffer buffer-name)))
    (if buffer
      (let ((window (get-buffer-window buffer)))
        (if window
          (select-window window)
          (message "Buffer %s is not displayed in any window" buffer-name)))
      (message "Buffer %s does not exist" buffer-name))))

(defun go-buffers ()
  "go buffers"
  (interactive)
  (list-buffers)
  (switch-to-buffer-window "*Buffer List*")
  )

(provide 'init-funcs)

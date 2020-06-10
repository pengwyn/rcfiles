(load "~/.emacs.d/init_extras_mc.el")
(load "~/.emacs.d/init_extras_greek.el")
(load "~/.emacs.d/init_extras_org.el")
(load "~/.emacs.d/init_extras_posframes.el")
(load "~/.emacs.d/init_extras_comments.el")
(load "~/.emacs.d/init_extras_julia.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Reload dir-locals automatically
;;----------------------------------

;; Stolen from https://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables
(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))
(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
   current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (my-reload-dir-locals-for-current-buffer)))))
(defun enable-autoreload-for-dir-locals ()
  (when (and (buffer-file-name)
             (equal dir-locals-file
                    (file-name-nondirectory (buffer-file-name))))
    (add-hook (make-variable-buffer-local 'after-save-hook)
              'my-reload-dir-locals-for-all-buffer-in-this-directory)))
(add-hook 'emacs-lisp-mode-hook 'enable-autoreload-for-dir-locals)



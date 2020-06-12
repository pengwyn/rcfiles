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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Better pasting 
;;

;; Redefine M-y to copy to clipboard and M-p to paste from clipboard
(evil-define-operator danny-evil-clip-yank (beg end type register yank-handler)
  (evil-yank beg end type ?+ yank-handler))
(evil-define-operator danny-evil-clip-paste (count &optional register yank-handler)
  (interactive "P<x>")
  (evil-paste-after 1 ?+ yank-handler))
(evil-define-key '(normal visual) 'global (kbd "C-M-y") 'danny-evil-clip-yank)
;; (evil-define-key '(normal insert) 'global (kbd "C-M-p") 'danny-evil-clip-paste)
(evil-define-key 'normal 'global (kbd "C-M-p") 'danny-evil-clip-paste)
(evil-define-key 'insert 'global (kbd "C-M-p") (lambda () (interactive) (evil-paste-from-register ?+)))
(evil-define-key '(normal insert) 'global (kbd "C-p") 'evil-paste-after)
(evil-define-key '(normal insert) 'global (kbd "M-P") 'evil-paste-pop)

;; * Killing interactively
;; Enable M-x kill-process (to kill the current buffer's process).
(put 'kill-process 'interactive-form
     '(interactive
       (let ((proc (get-buffer-process (current-buffer))))
         (if (process-live-p proc)
             (unless (yes-or-no-p (format "Kill %S? " proc))
               (error "Process not killed"))
           (error (format "Buffer %s has no process" (buffer-name))))
         nil)))


;; * Minibuffer stuff
;;----------------------------
(defvar my/evil-minibuffer-mode-map (make-sparse-keymap))
(define-minor-mode my/evil-minibuffer-mode
  :init-value nil
  :keymap my/evil-minibuffer-mode-map)
(add-hook 'minibuffer-setup-hook 'my/evil-minibuffer-mode)
(define-key my/evil-minibuffer-mode-map (kbd "C-r") 'evil-paste-from-register)

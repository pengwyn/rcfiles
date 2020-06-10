;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Frames over windows
;;----------------------------

(setq frames-over-windows t)

(setq display-buffer-alist
      `(("\\*Org Agenda\\*" . ((display-buffer-same-window)))
        ;;("\\*helpful.*:" . ((display-buffer-reuse-window display-buffer-reuse-mode-window display-buffer-below-selected)))
        ;; ("\\*helpful.*:" . ((display-buffer-below-selected)))
        ("\\*helpful.*:" . ((display-buffer-reuse-window display-buffer-pop-up-frame)))
        (,(rx (seq "*" (* nonl) "_region_" (* nonl) "*")) . ((display-buffer-no-window)))
        ("\\*julia\\*" . ((display-buffer-reuse-window display-buffer-pop-up-frame)
                          (reusable-frames . t) (inhibit-switch-frame . t)))
        ("\\*helm.*\\*" . ((display-buffer-pop-up-window)))
        (,(rx (or (seq "*" (* nonl) "*")
                  (seq string-start "magit" (* (not (any ":"))) ":")
                  (seq string-start "COMMIT"))) . ((display-buffer-reuse-window display-buffer-below-selected)
                                                   (reuseable-frames . t)))
        ;; ("\\*minibuffer\\*" (display-buffer-reuse-window))
        ))
(setq display-buffer-base-action '((display-buffer-reuse-window display-buffer-pop-up-frame)
                                   (reusable-frames . t)))


;; Need to special case helpful--describe for button pushes
(defun my/inwindow-helpful--describe (orig-fun &rest args)
  "Force helpful--describe to reuse the same window"
  (let ((display-buffer-overriding-action '(display-buffer-same-window)))
    (evil-set-jump)
    (apply orig-fun args)))
(advice-add 'helpful--describe :around #'my/inwindow-helpful--describe)
(setq evil--jumps-buffer-targets "\\*\\(new\\|scratch\\|helpful.*\\)\\*")

;; From https://emacs.stackexchange.com/questions/34343/attempt-to-delete-minibuffer-or-sole-ordinary-window
(defun my/delete-window-or-frame (&optional window frame force)
  (interactive)
  ;; Also delete the buffer
  (let ((buf (current-buffer)))
    (if (= 1 (length (window-list frame)))
      (delete-frame frame force)
      (delete-window window))
    (when (= 0 (length (get-buffer-window-list buf nil t)))
      (kill-buffer buf))))

(defun my/open-file-maybe (&optional path-in)
  ;; Bit of a weird thing - need to create a frame early so that it appears for
  ;; some of the startup modules like LSP asking questions.
  (let ((pop-up-frame-alist `((window-system . x)
                              (display . ,(getenv "DISPLAY")))))
    (if (and path-in (not (string= path-in "")))
        (let* ((path (file-truename path-in))
               (buf (get-file-buffer path)))
          (if buf
              (display-buffer buf)
            (make-frame '((window-system . x)))
            (display-buffer-same-window (find-file-noselect path) nil)))
      (make-frame '((window-system . x)))
      (funcall initial-buffer-choice))))


(define-key ctl-x-map "k" #'kill-this-buffer)
(define-key ctl-x-map "h" #'my/delete-window-or-frame-keep-buf)
(define-key evil-window-map (kbd "C-d") #'kill-buffer-and-window)
(define-key evil-window-map (kbd "C-c") #'my/delete-window-or-frame)
(define-key evil-window-map (kbd "c") #'my/delete-window-or-frame)
(define-key evil-window-map (kbd "C-n") #'make-frame-command)
(define-key evil-window-map (kbd "C-f") #'tear-off-window)

(global-set-key (kbd "C-x c") 'delete-frame)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Commenting things
;;----------------------------

(defun endless/comment-line (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above."
  (interactive "p")
  (let ((range (list (line-beginning-position)
                     (goto-char (line-end-position n)))))
    (comment-or-uncomment-region
     (apply #'min range)
     (apply #'max range)))
  ;; (forward-line 1)
  (back-to-indentation))

(global-set-key (kbd "C-;") 'endless/comment-line)
(setq-default comment-style 'multi-line)

;; This is copied from https://stackoverflow.com/questions/23588549/emacs-copy-region-line-and-comment-at-the-same-time
(defun copy-and-comment-region (beg end &optional arg)
  "Duplicate the region and comment-out the copied text.
See `comment-region' for behavior of a prefix arg."
  (interactive "r\nP")
  (copy-region-as-kill beg end)
  (goto-char end)
  (yank)
  (comment-region beg end arg))
(defun copy-and-comment-line (&optional arg)
  (interactive "P")
  (copy-and-comment-region (line-beginning-position) (line-end-position)))

(with-eval-after-load "evil" 
  (evil-define-key '(visual normal) 'global (kbd "C-y") 'copy-and-comment-region))


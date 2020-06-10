(use-package evil-mc
  :custom (evil-mc-one-cursor-show-mode-line-text nil)
  :config
  (global-evil-mc-mode 1)
  (evil-define-key '(normal visual) evil-mc-key-map (kbd "C-p") nil)
  (evil-define-key '(normal visual) evil-mc-key-map (kbd "C-n") nil)
  (evil-define-key 'normal evil-mc-key-map "\M-n" nil)
  (evil-define-key 'normal evil-mc-key-map "\M-p" nil)

  (defhydra hydra-evil-mc-keys ()
    "evilmckeys"
    ("\C-n" evil-mc-make-and-goto-next-match "make + next")
    ("\C-p" evil-mc-make-and-goto-prev-match "make + prev")
    ("M-n" evil-mc-skip-and-goto-next-match "skip + next")
    ("M-p" evil-mc-skip-and-goto-prev-match "skip + prev")
    ("q" evil-mc-undo-all-cursors "undo all" :exit t)
    ("n" evil-mc-make-and-goto-next-cursor "make + next cursor")
    ("p" evil-mc-make-and-goto-prev-cursor "make + prev cursor")
    ("N" evil-mc-skip-and-goto-next-cursor "skip + next cursor")
    ("P" evil-mc-skip-and-goto-prev-cursor "skip + prev cursor")
    ("\C-h" evil-mc-make-cursor-here "make here")
    ("S" evil-mc-pause-cursors "suspend")
    ("R" evil-mc-resume-cursors "continue")
    ("m" evil-mc-make-all-cursors "make all" :exit t)
    )

  (evil-define-key '(normal visual) evil-mc-key-map (kbd "M-m") 'hydra-evil-mc-keys/body)
  (evil-define-key '(normal visual) 'evil-mc-mode (kbd "M-m") 'hydra-evil-mc-keys/body)

  (defun my/make-evil-mc-cursor-on-click (event)
    "Stolen partially from the multiple cursor version code"
    (interactive "e")
    (mouse-minibuffer-check event)
    ;; Use event-end in case called from mouse-drag-region.
    ;; If EVENT is a click, event-end and event-start give same value.
    (let ((position (event-end event)))
      (if (not (windowp (posn-window position)))
          (error "Position not in text area of window"))
      (select-window (posn-window position))
      (let ((pt (posn-point position)))
        (if (numberp pt)
            ;; is there a fake cursor with the actual *point* right where we are?
            (save-excursion
              (goto-char pt)
              (evil-mc-make-cursor-here))))))
  (global-set-key (kbd "C-S-<mouse-1>") 'my/make-evil-mc-cursor-on-click)
  (global-set-key (kbd "C-S-<mouse-3>") 'evil-mc-undo-all-cursors)

  (defun col-at-point (point)
    (save-excursion (goto-char point) (current-column)))

  (defun evil--mc-make-cursor-at-col-append (_startcol endcol orig-line)
    (end-of-line)
    (when (> endcol (current-column))
      (insert-char ?\s (- endcol (current-column))))
    (move-to-column (- endcol 1))
    (unless (= (line-number-at-pos) orig-line)
      (evil-mc-make-cursor-here)))

  (defun evil--mc-make-cursor-at-col-insert (startcol _endcol orig-line)
    (end-of-line)
    (move-to-column startcol)
    (unless (or (= (line-number-at-pos) orig-line) (> startcol (current-column)))
      (evil-mc-make-cursor-here)))

  (defun evil--mc-make-vertical-cursors (beg end func)
    (evil-mc-pause-cursors)
    (apply-on-rectangle func
                        beg end (line-number-at-pos (point)))
    (evil-mc-resume-cursors)
    (evil-normal-state))

  (defun evil-mc-insert-vertical-cursors (beg end)
    (interactive (list (region-beginning) (region-end)))
    (evil--mc-make-vertical-cursors beg end 'evil--mc-make-cursor-at-col-insert)
    (move-to-column (min (col-at-point beg) (col-at-point end))))

  (defun evil-mc-append-vertical-cursors (beg end)
    (interactive (list (region-beginning) (region-end)))
    (evil--mc-make-vertical-cursors beg end 'evil--mc-make-cursor-at-col-append)
    (move-to-column (- (max (col-at-point beg) (col-at-point end)) 1)))

  (evil-define-key 'visual 'global "gI" 'evil-mc-insert-vertical-cursors)
  (evil-define-key 'visual 'global "gA" 'evil-mc-append-vertical-cursors)
  )

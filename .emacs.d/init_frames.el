;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Frames over windows
;;----------------------------

(setq frames-over-windows t)

(when frames-over-windows
  ;; This is needed on i3 as "iconified" makes the i3 window just stay there and
  ;; appear frozen.
  (setq frame-auto-hide-function 'delete-frame)

  (setq display-buffer-alist
        `(("\\*Org Agenda\\*" . ((display-buffer-same-window)))

          ("\\*helpful.*:" . ((display-buffer-use-some-frame display-buffer-pop-up-frame)
                              (frame-predicate . ,(lambda (frame) (string-equal (frame-parameter frame 'name) "*Helpful*")))
                              (pop-up-frame-parameters . ((name . "*Helpful*")))
                              ))

          ;; Latex region buffer, hide it.
          (,(rx (seq "*" (* nonl) "_region_" (* nonl) "*")) . ((display-buffer-no-window) (allow-no-window . t)))
          ("\\*julia\\*" . ((display-buffer-reuse-window display-buffer-pop-up-frame)
                            (reusable-frames . t) (inhibit-switch-frame . t)))

          ;; Helm should just pop up in the same frame
          ("\\*helm.*\\*" . ((display-buffer-pop-up-window)))

          ;; Earmuffs
          ;; (,(rx (or (seq "*" (* nonl) "*")
          ;;           (seq string-start "magit" (* (not (any ":"))) ":")
          ;;           (seq string-start "COMMIT"))
          ;;       ;; ) . ((display-buffer-reuse-window display-buffer-below-selected)
          ;;       ;;      (reuseable-frames . t))
          ;;       ) . ,display-buffer-fallback-action
          ;;           )
          (,(rx (or "*Help*"
                    (seq string-start "COMMIT")
                    (seq string-start "magit" (not (any ":")))))
           . ,display-buffer-fallback-action)

          ;; ("\\*minibuffer\\*" (display-buffer-reuse-window))
          ))
  (setq display-buffer-base-action '((display-buffer-reuse-window display-buffer-pop-up-frame)
                                     (reusable-frames . t)))


  (defvar named-frame-alist nil)
  (defun my/display-buffer-named-frame (buffer parameters-alist)
    "Display the buffer in the frame named in alist with `named-frame', if it exists."
    (let ((name (assoc 'named-frame parameters-alist)))
      (when name
        (let ((frame (assoc name named-frame-alist)))
          (when (frame-live-p frame)
            (display-buffer-use)))))
    )


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


  ;; make magit use a different frame
  (with-eval-after-load "magit"
    (define-key magit-file-section-map [remap magit-visit-thing]      'magit-diff-visit-file-other-frame)
    (define-key magit-hunk-section-map [remap magit-visit-thing]      'magit-diff-visit-file-other-frame))
  )




;; This is me attempt to fix, what appears like an incomplete implementation of
;; quit-restore behaviour. Add in a history storage of the quit-restore values
;; for the window.
;;
;; Changes happen at the default branch for the cond statement.
(defun quit-restore-window (&optional window bury-or-kill)
  "Quit WINDOW and deal with its buffer.
WINDOW must be a live window and defaults to the selected one.

According to information stored in WINDOW's `quit-restore' window
parameter either (1) delete WINDOW and its frame, (2) delete
WINDOW, (3) restore the buffer previously displayed in WINDOW,
or (4) make WINDOW display some other buffer than the present
one.  If non-nil, reset `quit-restore' parameter to nil.

Optional second argument BURY-OR-KILL tells how to proceed with
the buffer of WINDOW.  The following values are handled:

nil means to not handle the buffer in a particular way.  This
  means that if WINDOW is not deleted by this function, invoking
  `switch-to-prev-buffer' will usually show the buffer again.

`append' means that if WINDOW is not deleted, move its buffer to
  the end of WINDOW's previous buffers so it's less likely that a
  future invocation of `switch-to-prev-buffer' will switch to it.
  Also, move the buffer to the end of the frame's buffer list.

`bury' means that if WINDOW is not deleted, remove its buffer
  from WINDOW'S list of previous buffers.  Also, move the buffer
  to the end of the frame's buffer list.  This value provides the
  most reliable remedy to not have `switch-to-prev-buffer' switch
  to this buffer again without killing the buffer.

`kill' means to kill WINDOW's buffer."
  (setq window (window-normalize-window window t))
  (let* ((buffer (window-buffer window))
         (quit-restore (window-parameter window 'quit-restore))
         (prev-buffer
          (let* ((prev-buffers (window-prev-buffers window))
                 (prev-buffer (caar prev-buffers)))
            (and (or (not (eq prev-buffer buffer))
                     (and (cdr prev-buffers)
                          (not (eq (setq prev-buffer (cadr prev-buffers))
                                   buffer))))
                 prev-buffer)))
         (restore-history (nth 4 quit-restore))
         quad entry)
    (cond
     ((and (not prev-buffer)
           (or (eq (nth 1 quit-restore) 'frame)
               (and (eq (nth 1 quit-restore) 'window)
                    ;; If the window has been created on an existing
                    ;; frame and ended up as the sole window on that
                    ;; frame, do not delete it (Bug#12764).
                    (not (eq window (frame-root-window window)))))
           (eq (nth 3 quit-restore) buffer)
           ;; Delete WINDOW if possible.
           (window--delete window nil (eq bury-or-kill 'kill)))
      ;; If the previously selected window is still alive, select it.
      (when (window-live-p (nth 2 quit-restore))
        (select-window (nth 2 quit-restore))))
     ((and (listp (setq quad (nth 1 quit-restore)))
           (buffer-live-p (car quad))
           (eq (nth 3 quit-restore) buffer))
      ;; Show another buffer stored in quit-restore parameter.
      (when (and (integerp (nth 3 quad))
                 (if (window-combined-p window)
                     (/= (nth 3 quad) (window-total-height window))
                   (/= (nth 3 quad) (window-total-width window))))
        ;; Try to resize WINDOW to its old height but don't signal an
        ;; error.
        (condition-case nil
            (window-resize
             window
             (- (nth 3 quad) (if (window-combined-p window)
                                 (window-total-height window)
                               (window-total-width window)))
             (window-combined-p window t))
          (error nil)))
      (set-window-dedicated-p window nil)
      ;; Restore WINDOW's previous buffer, start and point position.
      (set-window-buffer-start-and-point
       window (nth 0 quad) (nth 1 quad) (nth 2 quad))
      ;; Deal with the buffer we just removed from WINDOW.
      (setq entry (and (eq bury-or-kill 'append)
                       (assq buffer (window-prev-buffers window))))
      (when bury-or-kill
        ;; Remove buffer from WINDOW's previous and next buffers.
        (set-window-prev-buffers
         window (assq-delete-all buffer (window-prev-buffers window)))
        (set-window-next-buffers
         window (delq buffer (window-next-buffers window))))
      (when entry
        ;; Append old buffer's entry to list of WINDOW's previous
        ;; buffers so it's less likely to get switched to soon but
        ;; `display-buffer-in-previous-window' can nevertheless find it.
        (set-window-prev-buffers
         window (append (window-prev-buffers window) (list entry))))
      ;; Reset the quit-restore parameter.
      ;; (set-window-parameter window 'quit-restore nil)
      ;; DANNY: pop the quit-restore parameter from the previous list
      (set-window-parameter window 'quit-restore restore-history)
      ;; Select old window.
      (when (window-live-p (nth 2 quit-restore))
        (select-window (nth 2 quit-restore))))
     (t
      ;; Show some other buffer in WINDOW and reset the quit-restore
      ;; parameter.
      ;; DANNY: pop the quit-restore parameter from the previous list
      (set-window-parameter window 'quit-restore restore-history)
      ;; Make sure that WINDOW is no more dedicated.
      (set-window-dedicated-p window nil)
      (switch-to-prev-buffer window bury-or-kill)))

    ;; Deal with the buffer.
    (cond
     ((not (buffer-live-p buffer)))
     ((eq bury-or-kill 'kill)
      (kill-buffer buffer))
     (bury-or-kill
      (bury-buffer-internal buffer)))))




(defun display-buffer-record-window (type window buffer)
  "Record information for window used by `display-buffer'.
TYPE specifies the type of the calling operation and must be one
of the symbols `reuse' (when WINDOW existed already and was
reused for displaying BUFFER), `window' (when WINDOW was created
on an already existing frame), or `frame' (when WINDOW was
created on a new frame).  WINDOW is the window used for or created
by the `display-buffer' routines.  BUFFER is the buffer that
shall be displayed.

This function installs or updates the quit-restore parameter of
WINDOW.  The quit-restore parameter is a list of four elements:
The first element is one of the symbols `window', `frame', `same' or
`other'.  The second element is either one of the symbols `window'
or `frame' or a list whose elements are the buffer previously
shown in the window, that buffer's window start and window point,
and the window's height.  The third element is the window
selected at the time the parameter was created.  The fourth
element is BUFFER."
  (let ((prev-quit-restore (window-parameter window 'quit-restore)))
  (cond
   ((eq type 'reuse)
    (if (eq (window-buffer window) buffer)
	;; WINDOW shows BUFFER already.  Update WINDOW's quit-restore
	;; parameter, if any.
	(let ((quit-restore (window-parameter window 'quit-restore)))
	  (when (consp quit-restore)
	    (setcar quit-restore 'same)
	    ;; The selected-window might have changed in
	    ;; between (Bug#20353).
	    (unless (or (eq window (selected-window))
                        (eq window (nth 2 quit-restore)))
	      (setcar (cddr quit-restore) (selected-window)))))
      ;; WINDOW shows another buffer.
      (with-current-buffer (window-buffer window)
	(set-window-parameter
	 window 'quit-restore
	 (list 'other
	       ;; A quadruple of WINDOW's buffer, start, point and height.
	       (list (current-buffer) (window-start window)
		     ;; Preserve window-point-insertion-type (Bug#12588).
		     (copy-marker
		      (window-point window) window-point-insertion-type)
		     (if (window-combined-p window)
                         (window-total-height window)
                       (window-total-width window)))
	       (selected-window) buffer
           prev-quit-restore)
     ))))
   ((eq type 'window)
    ;; WINDOW has been created on an existing frame.
    (set-window-parameter
     window 'quit-restore
     (list 'window 'window (selected-window) buffer prev-quit-restore)))
   ((eq type 'frame)
    ;; WINDOW has been created on a new frame.
    (set-window-parameter
     window 'quit-restore
     (list 'frame 'frame (selected-window) buffer prev-quit-restore))))))



;; And by default, kill these windows instead of burying them
;; (This will kicked off by magit-todos leaving rg processes stealing most of my CPU)
(defun advice/quit-window-kill (orig-func &rest args)
  (apply orig-func '(t)))
(advice-add 'quit-window :around 'advice/quit-window-kill)

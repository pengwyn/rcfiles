
(setq mypackages-path "~/.emacs.d/mypackages/")
(defun mypackages-p ()
  (file-directory-p mypackages-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Key bindings
;;----------------------------

(define-prefix-command 'danny-completions)

(evil-define-key '(insert normal) 'global (kbd "C-k") 'danny-completions)
(define-key danny-completions (kbd "C-l") 'evil-complete-previous-line)
(define-key danny-completions (kbd "C-o") 'helm-occur)
(define-key danny-completions (kbd "C-k") 'helm-resume)
(define-key danny-completions (kbd "C-a") 'helm-do-grep-ag)
(define-key danny-completions (kbd "C-d") (lambda () (interactive) 
                                            (let ((current-prefix-arg 4))
                                              (call-interactively #'ggtags-find-definition))))
(define-key danny-completions (kbd "C-r") 'ggtags-find-reference)
(define-key danny-completions (kbd "C-s") 'ggtags-find-other-symbol)
(define-key danny-completions (kbd "C-h") 'helm-navi-headings)
(define-key danny-completions (kbd "C-c") 'company-complete)
(define-key danny-completions (kbd "C-f") 'company-files)
(define-key danny-completions (kbd "q") 'evil-record-macro)
(define-key danny-completions (kbd "C-t") 'helm-magit-todos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Minor modes for general use
;;----------------------------

(use-package powerline
  :config
  (use-package powerline-evil)
  (powerline-center-evil-theme)
  ;; This is for my sanity
  (cl-loop for buf in (buffer-list)
           do (with-current-buffer buf
                (setq-local mode-line-format (default-value 'mode-line-format))
                (force-mode-line-update))))

(use-package mode-icons
  :config
  (mode-icons-mode))

;; Unfortunately, can't use this with lsp-ui-sideline
(use-package indent-guide
  :diminish indent-guide-mode
  :config
  (indent-guide-global-mode)
  (setq indent-guide-recursive t)
  (add-to-list 'indent-guide-inhibit-modes 'org-agenda-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (use-package yasnippet-snippets)
  :bind (:map yas-minor-mode-map
              ("C-M-y" . 'yas-expand)))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (use-package helm-flycheck)
  (use-package flycheck-pos-tip
    :config
    (flycheck-pos-tip-mode))
  )

(use-package flyspell)
;; Thsi isn't spelled correctly.

(use-package hl-todo
  :config
  (global-hl-todo-mode))
;; TODO: This is an example todo.

(use-package ace-window
  :bind (:map evil-window-map
         ("C-a" . ace-window)))

(use-package helm-swoop
  :bind (("C-/" . 'helm-swoop)
         :map undo-tree-map
         ("C-/" . nil)))
;; TODO: Make a hydra with C-? for the other swoop options.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Other evil things

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-exchange
  :custom
  (evil-exchange-key (kbd "zx"))
  :config
  (evil-exchange-install))

(use-package evil-avy
    :bind
    ("M-a" . avy-goto-char-timer))

(use-package evil-search-highlight-persist
  :demand t
  :bind (("C-l" . recenter-top-bottom-with-clear))
  :custom
  (evil-search-highlight-string-min-len 4)
  :config
  (global-evil-search-highlight-persist t)

  (defun recenter-top-bottom-with-clear ()
    "Do the normal recenter and redraw the screen."
    (interactive)
    (recenter-top-bottom)
    (evil-search-highlight-persist-remove-all)))


(use-package evil-mc
  :custom (evil-mc-one-cursor-show-mode-line-text nil)
  :config
  (global-evil-mc-mode 1)

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

  ;; Why do I need both of these - seems like black magic!
  (evil-define-key '(normal visual) evil-mc-key-map (kbd "M-m") 'hydra-evil-mc-keys/body)
  (evil-define-key '(normal visual) 'evil-mc-mode (kbd "M-m") 'hydra-evil-mc-keys/body)
  )

(use-package expand-region
  :bind (:map evil-visual-state-map
              ("v" . er/expand-region)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Size adjustments
;;----------------------------
(defun my/exit-text-scale-mode ()
  (interactive)
  (text-scale-mode 0))

(defhydra hydra-window-adjust ()
  "danny-window-adjust"
  ("-" text-scale-decrease "decrease")
  ("C--" text-scale-decrease nil)
  ("+" text-scale-increase "increase")
  ("=" text-scale-increase nil)
  ("C-=" text-scale-increase nil)
  ("0" my/exit-text-scale-mode "reset" :exit t)
  ("C-0" my/exit-text-scale-mode "reset" :exit t))
(global-set-key (kbd "C-x C--") 'hydra-window-adjust/text-scale-decrease)
(global-set-key (kbd "C-x C-+") 'hydra-window-adjust/text-scale-increase)
(global-set-key (kbd "C-x C-=") 'hydra-window-adjust/text-scale-increase)
(global-set-key (kbd "C-x C-0") 'hydra-window-adjust/my/exit-text-scale-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Useful major modes
;;----------------------------

;; ** Web mode

(use-package web-mode
  :custom ((web-mode-enable-engine-detect t))
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  )

;; ** Magit

(use-package magit
  :bind (("<f6>" . magit-status)
         ("<C-f6>" . my/magit-add-current-buffer))
  :config
  (use-package magit-popup)
  (use-package magit-todos)
  (use-package evil-magit)
  (use-package magit-todos
    :config
    (magit-todos-mode))

  ;; Stolen from https://stackoverflow.com/questions/40091077/equivalent-of-git-add-force-to-add-ignored-files-in-emacs-magit
  (defun my/magit-add-current-buffer ()
    "Adds (with force) the file from the current buffer to the git repo"
    (interactive)
    (shell-command (concat "git add -f "
                           (shell-quote-argument buffer-file-name)))
    (message "Added %s to git" buffer-file-name)
    )
  )

;; ** Helpful

(use-package helpful
  :demand t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key))
  :config
  ;; (when (boundp 'frames-over-windows)
  ;;   (evil-define-key 'normal helpful-mode-map "q" 'my/delete-window-or-frame))

  (with-eval-after-load 'helm-mode
    (dolist (func '(helpful-callable helpful-variable helpful-key)) 
      (add-to-list 'helm-completing-read-handlers-alist
                   (cons func 'helm-completing-read-symbols)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Completion

;; ** Boring but useful
(use-package hippie-exp
  :bind ("M-/" . hippie-expand))

;; ** Company

(use-package company
  :custom ((company-global-modes '(not shell-mode))
           (company-minimum-prefix-length 2)
           (company-idle-delay 0.2)
           (company-lighter-base " ")
           ;; Note that company-yasnippet is bad and never returns nil so the other
           ;; backends can never be used.  It's better to use the yas fallback options.
           (company-backends '( company-clang company-semantic company-gtags company-capf company-keywords company-dabbrev-code company-dabbrev))
           (company-dabbrev-time-limit 1.0)
           )
  :bind (:map company-mode-map
        ("C-<tab>" . company-other-backend))
  :config
  (global-company-mode)
  ;; Stolen from https://emacs.stackexchange.com/questions/13286/how-can-i-stop-the-enter-key-from-triggering-a-completion-in-company-mode
  ;;; Prevent suggestions from being triggered automatically. In particular,
  ;;; this makes it so that:
  ;;; - TAB will always complete the current selection.
  ;;; - RET will only complete the current selection if the user has explicitly
  ;;;   interacted with Company.
  ;;; - SPC will never complete the current selection.
  ;;;
  ;;; Based on:
  ;;; - https://github.com/company-mode/company-mode/issues/530#issue comment-226566961
  ;;; - https://emacs.stackexchange.com/a/13290/12534
  ;;; - http://stackoverflow.com/a/22863701/3538165
  ;;;
  ;;; See also:
  ;;; - https://emacs.stackexchange.com/a/24800/12534
  ;;; - https://emacs.stackexchange.com/q/27459/12534

  ;; <return> is for windowed Emacs; RET is for terminal Emacs
  (dolist (key '("<return>" "RET"))
    ;; Here we are using an advanced feature of define-key that lets
    ;; us pass an "extended menu item" instead of an interactive
    ;; function. Doing this allows RET to regain its usual
    ;; functionality when the user has not explicitly interacted with
    ;; Company.
    (define-key company-active-map (kbd key)
      `(menu-item nil company-complete-selection
                  :filter ,(lambda (cmd) (when (company-explicit-action-p) cmd)))))

  (define-key company-active-map (kbd "C-<return>") #'company-complete-selection)
  (define-key company-active-map (kbd "C-RET") #'company-complete-selection)
  (define-key company-active-map (kbd "S-TAB") #'company-complete-common-or-cycle)
  (define-key company-active-map [backtab] #'company-complete-common-or-cycle)
  (define-key company-active-map [S-iso-lefttab] #'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") nil)
  (define-key company-active-map [tab] nil)
  (use-package company-quickhelp
    :config
    (company-quickhelp-mode t))

  (use-package company-box
    :hook (company-mode . company-box-mode)
    :diminish company-box-mode
    :custom ((company-box-show-single-candidate t))
    :custom-face (company-box-scrollbar ((t (:background "bisque"))))
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Projectile
;;----------------------------
(use-package projectile
  :demand
  :bind (("<f9>" . danny-projectile)
         :map danny-projectile
         ("<f9>" . 'helm-projectile-switch-project)
         ("f" . 'helm-projectile-find-file-dwim)
         ;; ("p" . 'org-publish-current-project)
         ;; ("p" . 'projectile-compile-project)
         ("p" . 'my/projectile-compile)
         ("a" . 'helm-projectile-ag)
         ("d" . 'projectile-dired)
         ("x" . 'my/open-projectile-or-current-directory))

  :custom
  ((projectile-mode-line-prefix " ")
   (projectile-switch-project-action #'helm-projectile-find-file))
  :config
  (define-prefix-command 'danny-projectile)

  (projectile-mode)
  (use-package helm-projectile)
  (use-package projectile-direnv
    :hook (projectile-mode . projectile-direnv-export-variables))
  (use-package projectile-variable)

  (defun my/open-projectile-or-current-directory ()
    (interactive)
    (let ((directory (or (projectile-project-root)
                        (file-name-directory buffer-file-name))))
      (browse-url-xdg-open (file-truename directory))))
  
  (defun my/projectile-compile (arg)
    (interactive "P")
    (projectile-save-project-buffers)
    (let ((compilation-save-buffers-predicate 'ignore)
          (compilation-read-command nil))
      (projectile-compile-project arg)))

  (use-package ibuffer-projectile
    :config  
    (add-hook 'ibuffer-mode-hook
              (lambda () (ibuffer-projectile-set-filter-groups)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic))))
    (setq ibuffer-formats
          '((mark modified read-only " "
                  (name 18 18 :left :elide)
                  " "
                  (size 9 -1 :right)
                  " "
                  (mode 16 16 :left :elide)
                  " "
                  project-relative-file)
            (mark modified " "
                  (name)))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Treemacs
;;----------------------------
(use-package treemacs
  :bind (("<f8>" . treemacs-select-window)
         ("<M-f8>" . treemacs-add-and-display-current-project)
         :map treemacs-mode-map
         ([mouse-1] . treemacs-single-click-expand-action)
         ;; This should delete the treemacs window, but relies on (treemacs) behaviour.
         ("<return>" . (lambda (arg)
                         (interactive "P")
                         (call-interactively 'treemacs-RET-action)
                         (unless arg (treemacs)))))
  :config
  (use-package treemacs-evil)
  (use-package treemacs-projectile)
  (use-package treemacs-magit)

  (defun my/treemacs-is-file-tracked? (file git-info)
    (declare (side-effect-free t))
    (string= "?" (ht-get git-info file)))

  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)
  (add-to-list 'treemacs-pre-file-insert-predicates #'my/treemacs-is-file-tracked?)

  (defun my/treemacs-toggle-show-tracked-files (&optional force)
    "Toggle ignoring tracked files in treemacs"
    (interactive)
    (if (or (eq force -1)
            (and (not force) (member #'my/treemacs-is-file-tracked? treemacs-pre-file-insert-predicates)))
        (setq treemacs-pre-file-insert-predicates (delq #'my/treemacs-is-file-tracked? treemacs-pre-file-insert-predicates))
      (add-to-list 'treemacs-pre-file-insert-predicates #'my/treemacs-is-file-tracked?)
      (message "Added file-tracked to list")
      ))
  (define-key treemacs-toggle-map "t" #'my/treemacs-toggle-show-tracked-files)
  (define-key evil-treemacs-state-map "tt" #'my/treemacs-toggle-show-tracked-files)

  (treemacs-tag-follow-mode t)
  )

;; I think this is techinically separate
(use-package treemacs-icons-dired
  :config (treemacs-icons-dired-mode))

(use-package ibuffer-sidebar
  :after ibuffer treemacs
  :bind (("<C-f8>" . (lambda () (interactive)
                       (ibuffer-sidebar-toggle-sidebar)
                       (if (ibuffer-sidebar-showing-sidebar-p)
                           (call-interactively 'ibuffer-update)))))
  :config
  (add-hook 'ibuffer-sidebar-mode-hook
            (lambda () (setq-local ibuffer-filter-groups (ibuffer-projectile-generate-filter-groups))
               (setq-local ibuffer-current-format 1)
               (ibuffer-update-format)
               (ibuffer-redisplay t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Latex stuff
;;----------------------------
(use-package tex-mode
  :demand t
  :hook ((LaTeX-mode . auto-fill-mode)
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . latex-preview-pane-mode)
         (LaTeX-mode . (lambda () (add-hook 'after-save-hook 'preview-buffer nil t)))
         (LaTeX-mode . company-auctex-init)
         ;; (doc-view-mode . (lambda () (setq-local display-line-numbers nil)))
         (doc-view-mode . doc-view-fit-width-to-window))

  :custom
  ((latex-preview-pane-use-frame t)
   (doc-view-resolution 300)
   (preview-gs-options '("-q" "-dNOSAFER" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4")))

  :config
  (use-package latex-math-preview)
  (use-package latex-pretty-symbols)
  (use-package latex-preview-pane)
  (use-package company-auctex)

  (TeX-global-PDF-mode nil)

  (unbind-key "k" image-mode-map)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * ggtags
;;----------------------------
(use-package ggtags
  :demand t
  :hook (c-mode-common . (lambda () (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                            (ggtags-mode 1)
                            (evil-define-key 'normal (current-local-map) (kbd "M-.") 'ggtags-find-tag-dwim))
                          ))
  :bind (:map ggtags-mode-map
              ("C-c g s" . ggtags-find-other-symbol)
              ("C-c g h" . ggtags-view-tag-history)
              ("C-c g r" . ggtags-find-reference)
              ("C-c g f" . ggtags-find-file)
              ("C-c g c" . ggtags-create-tags)
              ("C-c g u" . ggtags-update-tags)

              ("M-," . pop-tag-mark)
              ("C-M-," . ggtags-find-tag-continue))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * THEME
;;----------------------------

(use-package unicode-fonts
  :custom
  (unicode-fonts-fallback-font-list '("Symbola" "Quivira" "DejaVu Math Tex Gyre"))
  :config
  (unicode-fonts-setup)
  )

(use-package moe-theme
  :config
  (load-theme 'moe-dark t nil)
  (custom-theme-set-faces 'moe-dark '(default ((t (:background "#000000" :foreground "#c6c6c6" :family "mononoki Nerd Font" :height 100)))))
  (custom-theme-set-faces 'moe-dark '(compilation-error ((t (:foreground "#333" :background "#faa" :weight bold))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Random things
;;----------------------------

;; ** Good to have these major modes
;; (use-package pkgbuild-mode)
(use-package yaml-mode)
(use-package csv-mode)

(use-package markdown-mode
  :hook ((markdown-mode-hook . auto-fill-mode)
         (markdown-mode-hook . (lambda () (setq-local auto-fill-function #'markdown-fill-paragraph))))
  :config
  (use-package poly-markdown))

;; *** Dashboard

(use-package dashboard
  :custom
  ((dashboard-items '((recents . 10)
                     (bookmarks . 10)
                     ;;(project . 5)
                     ;; (agenda . 15)
                     (registers . 5)
    				 ))
   (show-week-agenda-p t))
  :config
  (my/evil-add-bindings dashboard-mode-map)
  (evil-set-initial-state 'dashboard-mode 'emacs)
   
  (defun my/select-dashboard ()
    (if (get-buffer dashboard-buffer-name)
        (dashboard-refresh-buffer)
      (dashboard-insert-startupify-lists)
      (switch-to-buffer dashboard-buffer-name nil t)))
    
  (setq initial-buffer-choice (lambda () (or (my/select-dashboard)
                                             (get-buffer "*scratch*"))))
  (setq inhibit-startup-screen t)
  )

;; *** Term mode

(use-package term
  :hook (term-mode . my-term-mode)
        (term-mode . eterm-256color-mode)
  :bind (:map my-term-mode-map
              ("<f5>" . evil-window-mru)
              ("C-w" . evil-window-map)
              ("C-d" . term-send-eof)
              ("C-x C-c" . save-buffers-kill-terminal)
              ("C-c" . term-interrupt-subjob)
              ("<up>" . my/term-send-up)
              ("<down>" . my/term-send-down))

  :config
  (defvar my-term-mode-map (make-sparse-keymap))
  (define-minor-mode my-term-mode
    :init nil
    :keymap my-term-mode-map)
  (add-hook 'term-exec-hook (lambda () (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))

  ;; Redefine the basic up and down because it doesn't work well with REPLs and history
  (defun my/term-send-up    () (interactive) (term-send-raw-string "\e[A"))
  (defun my/term-send-down  () (interactive) (term-send-raw-string "\e[B"))

  (use-package eterm-256color)

  (evil-set-initial-state 'term-mode 'emacs))


;; *** ediff
(setq-default ediff-diff-options "-w")
(setq-default ediff-split-window-function 'split-window-horizontally)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)


;; *** evil-numbers, even though I never use this

(use-package evil-numbers
  :bind
    ("C-c +" . evil-numbers/inc-at-pt)
    ("C-c -" . evil-numbers/dec-at-pt))

;; ** Minor modes

(use-package all-the-icons)


;; *** sudo saving
(use-package sudo-edit
  :config
  (defun sudo-save ()
    (interactive)
    (if (not buffer-file-name)
        (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
      (write-file (concat "/sudo:root@localhost:" buffer-file-name)))))


;; *** Extension for zx
;;----------------------------
(defun my/create-and-exchange (name)
  "Create a new variable name and exchange it with the previously selected text."
  (interactive "M")
  (insert name " = ")
  (let ((beg (point)))
    (insert name)
    (let ((end (point)))
      (message "Beg/end: %d %d" beg end)
      (evil-exchange beg end :inclusive)
      )))
(evil-define-key 'insert 'global (kbd "C-x C-z") 'my/create-and-exchange)
;; TODO: Improve this with an overlay to show what will be done.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *** Outshine/outorg/navi-mode
;;----------------------------

(use-package outshine
  :hook ((prog-mode . outline-minor-mode)
         (outline-minor-mode . (lambda () (outshine-mode)
                                  (setq-local outshine-imenu-preliminary-generic-expression
                                              `((nil ,(concat (outshine-calc-outline-regexp) "\\(.*$\\)") 1))))))
  :diminish outline-minor-mode
  :diminish outshine-mode
  :config

  ;; Fix self-insert-command for evil-mc
  (add-to-list 'evil-mc-custom-known-commands '(outshine-self-insert-command . ((:default . evil-mc-execute-default-call-with-count))))

  :custom-face
  (outline-1 ((t (:height 2.0 :family "Liberation Mono" :foreground "black" :background "DarkSeaGreen1"))))
  (outline-2 ((t (:height 1.5 :family "Liberation Mono"))))
  (outline-3 ((t (:height 1.2 :family "Liberation Mono"))))
  (org-agenda-dimmed-todo-face ((t (:foreground "grey20"))))

  :custom
  (outshine-imenu-show-headlines-p nil)

  (defvaralias 'outline-promotion-headings 'outshine-promotion-headings)
  )


;; ** Other things

;; *** Visit files with line numbers

(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
  "looks for filenames like file:line or file:line:position and reparses name in such manner that position in file"
  (ad-set-arg 0
              (mapcar (lambda (fn)
                        (let ((name (car fn)))
                          (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                              (cons
                               (match-string 1 name)
                               (cons (string-to-number (match-string 2 name))
                                     (string-to-number (or (match-string 3 name) "")))
                               )
                            fn))) files))
  )



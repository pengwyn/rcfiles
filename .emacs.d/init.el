;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Emacs customize
;;----------------------------
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Packages
;;----------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa stable" . "https://stable.melpa.org/packages/"))
                                        ;(add-to-list 'package-archives
                                        ;             '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(setq package-archive-priorities
      '(("melpa stable" . 0)
        ("gnu" . -10)
        ("melpa" . 5)))

(package-initialize)

(global-set-key (kbd "<f2>") 'package-list-packages)
(global-set-key (kbd "<f3>") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "<f4>") 'save-buffers-kill-emacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * use-package
;;----------------------------

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package diminish)
(use-package bind-key)

(use-package use-package-hydra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Diminish basic modes
;;----------------------------

(use-package undo-tree
  :custom (undo-tree-enable-undo-in-region nil))

;; (diminish 'undo-tree-mode)
(diminish 'abbrev-mode)
(diminish 'auto-fill-function)
(diminish 'auto-revert-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * General stuff
;;----------------------------

(setq x-select-enable-clipboard nil)

(setq-default inhibit-startup-message t ;; hide the startup message
              ;; mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
              mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
              mouse-wheel-follow-mouse 't ;; scroll window under mouse
              ;; scroll-step 1 ;; keyboard scroll one line at a time
              scroll-conservatively 101 ;; keyboard scroll one line at a time
              scroll-margin 3
              split-height-threshold 2
              split-width-threshold 20
              fill-column 80
              recentf-max-saved-items 1000
              help-window-select t
              ;; display-line-numbers 'visual
              display-line-numbers nil
              indent-tabs-mode nil
              tab-width 4
              dired-auto-revert-buffer t
              sentence-end-double-space nil)


(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(global-set-key (kbd "C-x c") 'delete-frame)
(global-set-key (kbd "M-w") 'ace-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Util functions
;;----------------------------

;; Stolen from somewhere.
(defmacro define-key-with-fallback (keymap key def condition &optional mode)
  "Define key with fallback. Binds KEY to definition DEF in keymap KEYMAP, 
   the binding is active when the CONDITION is true. Otherwise turns MODE off 
   and re-enables previous definition for KEY. If MODE is nil, tries to recover 
   it by stripping off \"-map\" from KEYMAP name."
  `(define-key ,keymap ,key
     (lambda () (interactive)
       (if ,condition ,def
         (let* ((,(if mode mode
                    (let* ((keymap-str (symbol-name keymap))
                           (mode-name-end (- (string-width keymap-str) 4)))
                      (if (string= "-map" (substring keymap-str mode-name-end))
                          (intern (substring keymap-str 0 mode-name-end))
                        (error "Could not deduce mode name from keymap name (\"-map\" missing?)")))) 
                 nil)
                (original-func (key-binding ,key)))
           (message "About to call %s" original-func)
           (call-interactively original-func))))))


(defun my/evil-add-bindings (map)
  "Adds the usual evil hjkl and some other things I like"
  (evil-add-hjkl-bindings map 'emacs
    (kbd "/") 'evil-search-forward
    (kbd "n") 'evil-search-next
    (kbd "N") 'evil-search-previous
    (kbd "^") 'evil-first-non-blank
    (kbd "^") 'evil-end-of-line
    (kbd "C-f") 'evil-scroll-page-down
    (kbd "C-b") 'evil-scroll-page-up))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Switch back to minibuffer
;;------------------------------------------------

;; Emergency switch back to minibuffer
;; Stolen from http://superuser.com/questions/132225/how-to-get-back-to-an-active-minibuffer-prompt-in-emacs-without-the-mouse
(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))
(global-set-key (kbd "<f12>") 'switch-to-minibuffer-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Align func
;;----------------------------
(defun my/simple-align (beg end)
     "Align using any 2+ space with 4 spaces"
     (interactive "r")
     (align-regexp beg end (rx (group (or
                                       (>= 2 (syntax whitespace))
                                       (group (* (syntax whitespace)) (1+ "\t") (* (syntax whitespace))))
                                      )) 1 4 t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Evil stuff
;;----------------------------


(use-package evil
  :demand t

  :custom ((evil-cross-line t)
           (evil-visual-x-select-timeout 999)
           (evil-symbol-word-search 'symbol)
           (evil-want-fine-undo t)
           (evil-emacs-state-cursor '("red" box))
           (evil-normal-state-cursor '("green" box))
           (evil-visual-state-cursor '("orange" box))
           (evil-insert-state-cursor '("red" bar))
           (evil-replace-state-cursor '("red" bar))
           (evil-operator-state-cursor '("red" hollow)))

  :bind (:map evil-window-map
         ("C-l" . evil-window-right)
         ("C-h" . evil-window-left)
         ("C-k" . evil-window-up)
         ("C-j" . evil-window-down)
         ("C-w" . evil-window-mru)
         ("C-a" . ace-window)
         ("C-d" . kill-buffer-and-window)
         ("C-c" . my/delete-window-or-frame)
         ("c" . my/delete-window-or-frame)
         ("C-n" . make-frame-command)
         ("C-f" . tear-off-window)

         :map evil-visual-state-map
         (">" . (lambda ()
                  (interactive)
                                   ; ensure mark is less than point
                  (when (> (mark) (point)) 
                    (exchange-point-and-mark)
                    )
                  (evil-normal-state)
                  (evil-shift-right (mark) (point))
                  (evil-visual-restore) ; re-select last visual-mode selection
                  ))

         ("<" . (lambda ()
                  (interactive)
                                   ; ensure mark is less than point
                  (when (> (mark) (point)) 
                    (exchange-point-and-mark)
                    )
                  (evil-normal-state)
                  (evil-shift-left (mark) (point))
                  (evil-visual-restore) ; re-select last visual-mode selection
                  ))
         ("n" . (lambda (beg end)
                  (interactive "r")
                  (evil-search (filter-buffer-substring beg end) t)))
         ("n" . (lambda (beg end)
                  (interactive "r")
                  (my/evil-search-visual beg end t)))
         ("N" . (lambda (beg end)
                  (interactive "r")
                  (my/evil-search-visual beg end nil)))
         :map evil-emacs-state-map
         ("C-w" . evil-window-map)
         :map evil-normal-state-map
         )

  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  
  :config
  (evil-mode 1)

  (use-package evil-collection
    :config
    (evil-collection-init))

  (evil-global-set-key 'insert (kbd "<C-n>") nil)
  (evil-global-set-key 'insert (kbd "<C-p>") nil)
  (evil-global-set-key 'motion "K" nil)
  (evil-global-set-key 'normal "K" nil)
  (evil-global-set-key 'motion (kbd "<down-mouse-1>") nil)

  (evil-mode 1)
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (my/evil-add-bindings package-menu-mode-map)
  ;; (eval-after-load 'ibuffer '(my/evil-add-bindings ibuffer-mode-map))
  ;; Don't want to override / in the ibuffer map
  (eval-after-load 'ibuffer '(evil-add-hjkl-bindings ibuffer-mode-map 'emacs))
  

  (defun my/evil-search-visual (beg end forward)
    "Search using the current visual selection"
    (let ((text (regexp-quote (filter-buffer-substring beg end))))
      (evil-exit-visual-state)
      ;;(evil-push-search-history text forward)
      (isearch-update-ring text t)
      (evil-search text forward)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; *** Minibuffer stuff
  ;;----------------------------
  (defvar my/evil-minibuffer-mode-map (make-sparse-keymap))
  (define-minor-mode my/evil-minibuffer-mode
    :init-value nil
    :keymap my/evil-minibuffer-mode-map)
  (add-hook 'minibuffer-setup-hook 'my/evil-minibuffer-mode)
  (define-key my/evil-minibuffer-mode-map (kbd "C-r") 'evil-paste-from-register)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; *** Misc
  ;;----------------------------

  (defun recenter-top-bottom-with-clear ()
    "Do the normal recenter and redraw the screen."
    (interactive)
    (recenter-top-bottom)
    (evil-search-highlight-persist-remove-all))

  (dolist (map '(evil-normal-state-map evil-insert-state-map evil-motion-state-map))
    ;; (define-key (eval map) (kbd "C-w") 'evil-window-map)
    (define-key (eval map) (kbd "C-c +") 'evil-numbers/inc-at-pt)
    (define-key (eval map) (kbd "C-c -") 'evil-numbers/dec-at-pt)

    (define-key (eval map) (kbd "RET") nil)
    (define-key (eval map) (kbd " ") nil)

    (define-key (eval map) (kbd "C-.") nil)
    (define-key (eval map) (kbd "M-.") nil)

    (dolist (key '("C-x C-<space>" "C-x <space>" "C-l"))
      (define-key (eval map) (kbd key) 'recenter-top-bottom-with-clear))
    )
                                        ; Redefine M-y to copy to clipboard and M-p to paste from clipboard
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

  (use-package evil-magit)
  
  (use-package evil-surround
    :config
    (global-evil-surround-mode 1))
  
  (use-package evil-exchange
    :custom
    (evil-exchange-key (kbd "zx"))
    :config
    (evil-exchange-install)
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
    )

  (use-package evil-avy
    :bind
    ;; Was originally only for normal mode.
    ("M-a" . avy-goto-char-timer))
  
  (use-package evil-search-highlight-persist
    :custom
    (evil-search-highlight-string-min-len 3)
    :config
    (global-evil-search-highlight-persist t))

  (use-package evil-visualstar)
  (use-package evil-numbers)
  (use-package evil-visualstar)

  (use-package evil-anzu
    :bind (:map evil-insert-state-map
                ("C-M-r" . anzu-query-replace-regexp)
                :map evil-normal-state-map           
                ("C-M-r" . anzu-query-replace-regexp)))

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
    ;; I don't want the original bindings anymore - but I don't know how to get rid of them!
    ;; The below doesn't get rid of the evil key bindings unfortunately.
    ;; (setq evil-mc-key-map (make-sparse-keymap))

    ;; (evil-define-key '(normal visual) evil-mc-key-map (kbd "g r") 'hydra-evil-mc-keys/body)
    (evil-define-key '(normal visual) evil-mc-key-map (kbd "M-m") 'hydra-evil-mc-keys/body)

    (defun danny-make-evil-mc-cursor-on-click (event)
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
    (global-set-key (kbd "C-S-<mouse-1>") 'danny-make-evil-mc-cursor-on-click)
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
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Short modes
;;----------------------------

(savehist-mode t)
(save-place-mode t)
(show-paren-mode t)
(global-auto-revert-mode t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(use-package powerline
  :config
  (use-package powerline-evil)
  (setq powerline-display-hud nil)
  (setq powerline-display-buffer-size nil)
  (setq powerline-gui-use-vcs-glyph t)

  (powerline-center-evil-theme)
  ;; Based on powerline-center-evil-theme
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face0 (if active 'powerline-active0 'powerline-inactive0))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" face0 'l)
                                     ;;
                                     ;; (when powerline-display-buffer-size
                                     ;;   (powerline-buffer-size face0 'l))
                                     (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                                     (powerline-raw " " face0)
                                     (funcall separator-left face0 face1)
                                     (powerline-narrow face1 'l)
                                     ;;
                                     ;; (powerline-vc face1)
                                     ))
                          (rhs (list (powerline-raw global-mode-string face1 'r)
                                     ;;
                                     ;; (powerline-raw "%4l" face1 'r)
                                     ;; (powerline-raw ":" face1)
                                     ;; (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 face0)
                                     (powerline-raw " " face0)
                                     (powerline-raw "%3p" face0 'r)
                                     ;;
                                     ;; (when powerline-display-hud
                                     ;;   (powerline-hud face2 face1))
                                     ))
                          (center (append (list (powerline-raw " " face1)
                                                (funcall separator-left face1 face2)
                                                (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                                  (powerline-raw erc-modified-channels-object face2 'l))
                                                (powerline-major-mode face2 'l)
                                                (powerline-process face2)
                                                (powerline-raw " " face2))
                                          (if (split-string (format-mode-line minor-mode-alist))
                                              (append (if evil-mode
                                                          (list (funcall separator-right face2 face1)
                                                                (powerline-raw evil-mode-line-tag face1 'l)
                                                                (powerline-raw " " face1)
                                                                (if current-input-method
                                                                    (powerline-raw current-input-method face1 'l)
                                                                    (powerline-raw " " face1))
                                                                (funcall separator-left face1 face2)))
                                                      (list (powerline-minor-modes face2 'l)
                                                            (powerline-raw " " face2)
                                                            (funcall separator-right face2 face1)))
                                            (list (powerline-raw evil-mode-line-tag face2)
                                                  (funcall separator-right face2 face1))))))
                     (concat (powerline-render lhs)
                             (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                             (powerline-render center)
                             (powerline-fill face1 (powerline-width rhs))
                             (powerline-render rhs)
                             )))))
  )

(use-package magit
  :bind (("<f6>" . magit-status)
         ("<C-f6>" . my/magit-add-current-buffer)
         :map magit-mode-map
         ("q" . (lambda () (interactive) (magit-mode-bury-buffer 16))))
  :config
  (use-package magit-popup)
  (use-package magit-todos)

  (defun my/setup-gpg-agent (&optional force)
    "Setup gpg agent env variables for magit."
    (unless (or force (getenv "SSH_AUTH_SOCK"))
      (with-temp-buffer
        (insert-file-contents "~/.gnupg/evalstr")
        (let* ((found-str (condition-case nil
                              (progn
                                (search-forward-regexp "SSH_AUTH_SOCK=[^\s;]*")
                                (match-string 0))
                            (error (progn (message "Unable to find SSH_AUTH_SOCK string!")
                                          "")
                                   )))
               (envvar (split-string found-str "=")))
          (when envvar
            (apply 'setenv envvar))
          )))
    (call-process-shell-command "gpg-connect-agent updatestartuptty /bye")
    )
  (add-hook 'magit-credential-hook 'my/setup-gpg-agent)


  ;; Stolen from https://stackoverflow.com/questions/40091077/equivalent-of-git-add-force-to-add-ignored-files-in-emacs-magit
  (defun my/magit-add-current-buffer ()
    "Adds (with force) the file from the current buffer to the git repo"
    (interactive)
    (shell-command (concat "git add -f "
                           (shell-quote-argument buffer-file-name)))
    (message "Added %s to git" buffer-file-name)
    )

  )

(use-package web-mode
  :custom ((web-mode-enable-engine-detect t))
  
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  )

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (use-package yasnippet-snippets)
  ;; :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map
              ("C-M-y" . 'yas-expand)))

(use-package mode-icons
  :if (or (display-graphic-p) (daemonp))
  :config
  (setq mode-icons (delete (seq-find (lambda (x) (let ((y (pop x)))
                                                   (and (string-or-null-p y)
                                                        (string-match-p (regexp-quote "company") y))))
                                     mode-icons)
                           mode-icons))
  (add-to-list 'mode-icons '("company-box"  61869 FontAwesome))
  ;; (mode-icons-mode)
  (add-hook 'after-make-frame-functions (lambda (frame) (mode-icons-mode)))
  )

(use-package php-mode
  :config
  (use-package company-php))

(use-package flycheck
  :config
  (use-package helm-flycheck)
  ;; TODO: Fix the Lint.jl package
  (use-package flycheck-julia
    :config
    (flycheck-julia-setup)))

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

(use-package helpful
  :demand t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key))
  :config
  ;; (evil-define-key 'normal helpful-mode-map "q" 'delete-window)
  (evil-define-key 'normal helpful-mode-map "q" 'my/delete-window-or-frame)

  (with-eval-after-load 'helm-mode
    (dolist (func '(helpful-callable helpful-variable helpful-key)) 
      (add-to-list 'helm-completing-read-handlers-alist
                   (cons func 'helm-completing-read-symbols)))))


;; (use-package sublimity
;;   :custom
;;   ((sublimity-map-size 20)
;;    (sublimity-map-fraction 0.3)
;;    (sublimity-map-text-scale -7))
                       
;;   :config
;;   (require 'sublimity-map)
;;   (sublimity-mode 1)
;;   )

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package magit-todos
  :after magit
  :after hl-todo
  (magit-todos-mode))

(use-package ibuffer
  :config

  (use-package ibuffer-projectile
    :config  
    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (message "In the ibuffer mode hook")
                (ibuffer-projectile-set-filter-groups)
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

  (use-package ibuffer-sidebar
    :bind (("C-<f8>" . (lambda ()
                         (interactive)
                         (ibuffer-sidebar-toggle-sidebar)
                         (if (ibuffer-sidebar-showing-sidebar-p)
                             (call-interactively 'ibuffer-update)))))
    :config
    (add-hook 'ibuffer-sidebar-mode-hook
              (lambda ()
                (setq-local ibuffer-filter-groups (ibuffer-projectile-generate-filter-groups))
                (setq-local ibuffer-current-format 1)
                (ibuffer-update-format)
                (ibuffer-redisplay t))))
  )

(use-package highlight-symbol)

(use-package hydra)
(use-package ace-window)
(use-package sudo-edit)
(use-package pkgbuild-mode)
(use-package yaml-mode)
(use-package better-defaults) 
(use-package switch-window)
(use-package csv-mode)
(use-package all-the-icons)
;; Unfortunately, can't use this with lsp-ui-sideline
;; (use-package indent-guide
;;   :diminish indent-guide-mode
;;   :config
;;   (indent-guide-global-mode)
;;   (setq indent-guide-recursive t)
;;   (add-to-list 'indent-guide-inhibit-modes 'org-agenda-mode))


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
  (define-key evil-visual-state-map (kbd "C-y") 'copy-and-comment-region)
  ;; (define-key evil-insert-state-map (kbd "C-y") 'copy-and-comment-line)
  (define-key evil-normal-state-map (kbd "C-y") 'copy-and-comment-line))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Prettify
;;----------------------------

(use-package prettify-greek
  :custom
  (prettify-symbols-unprettify-at-point "right-edge"))

                                        ; Make up my own set with \ in front of them.
(defconst danny-prettify-set 
  (let* ((my-greek (append (copy-tree prettify-greek-lower) (copy-tree prettify-greek-upper))))
    (dolist (item my-greek)
      (setcar item (concat "\\" (car item))))
    (append my-greek prettify-greek-lower prettify-greek-upper)))

(defun danny-prettify-predicate (start end _match) "Only care about words and not symbols."
       ;; (not (or (= (char-syntax (char-after end)) ?w)
       ;;           (= (char-syntax (char-before start)) ?w))))
       (not (or (string-match-p "[a-zA-Z]" (string (char-after end)))
                (string-match-p "[a-zA-Z]" (string (char-before start))))))


(defun danny-add-prettify-greek (mode) "Add prettify-greek symbols to mode."
       (add-hook mode (lambda ()
                        (setq
                                        ;prettify-symbols-alist (append prettify-symbols-alist prettify-greek-lower prettify-greek-upper)))))
                         prettify-symbols-alist (append prettify-symbols-alist danny-prettify-set))
                        (prettify-symbols-mode t)
                        (setq prettify-symbols-compose-predicate 'danny-prettify-predicate))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Elisp
;;----------------------------

(evil-define-key 'visual lisp-mode-shared-map (kbd "C-M-x") 'eval-region)
(add-hook 'lisp-mode-hook 'highlight-symbol-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Sudo edit
;;----------------------------
(defun sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;;;;;;;;;;;;;;;
;; * Company stuff

(use-package company
  :hook (after-init . global-company-mode)
  :custom ((company-global-modes '(not shell-mode))
           (company-minimum-prefix-length 2)
           (company-idle-delay 0.2)
           (company-lighter-base " ")
           ;; Note that company-yasnippet is bad and never returns nil so the other
           ;; backends can never be used.  It's better to use the yas fallback options.
           (company-backends '( company-clang company-semantic company-gtags company-capf company-keywords company-dabbrev-code company-dabbrev))
           (company-dabbrev-time-limit 1.0))
  :bind
  ;;(define-key company-mode-map (kbd "C-M-i") 'company-complete)
  (:map company-mode-map
        ("C-<tab>" . company-other-backend))
  :config
  ;; Stolen from https://emacs.stackexchange.com/questions/13286/how-can-i-stop-the-enter-key-from-triggering-a-completion-in-company-mode
  ;;; Prevent suggestions from being triggered automatically. In particular,
  ;;; this makes it so that:
  ;;; - TAB will always complete the current selection.
  ;;; - RET will only complete the current selection if the user has explicitly
  ;;;   interacted with Company.
  ;;; - SPC will never complete the current selection.
  ;;;
  ;;; Based on:
  ;;; - https://github.com/company-mode/company-mode/issues/530#issuecomment-226566961
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
                  :filter ,(lambda (cmd)
                             (when (company-explicit-action-p)
                               cmd)))))
  ;; (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  ;; (define-key company-active-map (kbd "SPC") nil)

  ;; Company appears to override the above keymap based on company-auto-complete-chars.
  ;; Turning it off ensures we have full control.
  ;; (setq company-auto-complete-chars nil)
  (define-key company-active-map (kbd "C-<return>") #'company-complete-selection)
  (define-key company-active-map (kbd "C-RET") #'company-complete-selection)
  (define-key company-active-map (kbd "S-TAB") #'company-complete-common-or-cycle)
  (define-key company-active-map [backtab] #'company-complete-common-or-cycle)
  (define-key company-active-map [S-iso-lefttab] #'company-complete-common-or-cycle)
  ;; (define-key company-active-map [?\C-\t] #'company-complete-common)
  (define-key company-active-map (kbd "TAB") nil)
  (define-key company-active-map [tab] nil)
  (use-package company-quickhelp
    :config
    (company-quickhelp-mode t))

  (when (display-graphic-p)
    (use-package company-box
        :hook (company-mode . company-box-mode)
        ;; :diminish company-box-mode
        ))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Helm stuff
;;----------------------------
(use-package helm
  ;; :diminish helm-mode
  :init
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  :config
  (helm-mode 1)
  (require 'helm-config)
  (when (executable-find "curl")
    (setq-default helm-google-suggest-use-curl-p t))
  (use-package wgrep-helm)
  (use-package helm-ag)

  (defun helm-ff-sort-by-size ()
    (interactive)
    (setq helm-ff-initial-sort-method 'size)
    (helm-update))

  (defun helm-ff-sort-by-newest ()
    (interactive)
    (setq helm-ff-initial-sort-method 'newest)
    (helm-update))

  (defun helm-ff-sort-alpha ()
    (interactive)
    (setq helm-ff-initial-sort-method nil)
    (helm-update))

  :custom ((helm-split-window-in-side-p t) ; open helm buffer inside current window, not occupy whole other window
           (helm-move-to-line-cycle-in-source t) ; move to end or beginning of source when reaching top or bottom of source.
           (helm-ff-search-library-in-sexp t) ; search for library in `require' and `declare-function' sexp.
           (helm-scroll-amount 8) ; scroll 8 lines other window using M-<next>/M-<prior>
           (helm-ff-file-name-history-use-recentf t)
           (helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
           (helm-semantic-fuzzy-match t)
           (helm-imenu-fuzzy-match t))

  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  :bind (("C-x c" . nil)
         ("M-x" . helm-M-x)
         ;; (global-set-key (kbd "C-M-x") 'execute-extended-command)
         ;; (global-set-key (kbd "C-x b") 'helm-mini)
         ("C-x f" . helm-find-files)
         ("C-x C-f" . helm-multi-files)
         ("C-x b" . helm-buffers-list)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
         ("C-i" . helm-execute-persistent-action) ; make TAB work in terminal
         ("C-z"  . helm-select-action) ; list actions using C-z
         :map helm-grep-map
         ("C-." . 'helm-goto-next-file)
         ("C-," . 'helm-goto-precedent-file)
         :map helm-find-files-map
         ("S-<f1>" . 'helm-ff-sort-by-size)
         ("S-<f2>" . 'helm-ff-sort-by-newest)
         ("S-<f3>" . 'helm-ff-sort-alpha)))

;; TODO: Add several alt-choices to the multifiles, including
;; - Open file in vertical for C-x C-f
;; - Open dired in split

;; Kind of helm related
(use-package tramp
  :config
  (setenv "SHELL" "/bin/bash"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * ggtags
;;----------------------------
(use-package ggtags
  :demand t
  :hook (c-mode-common .
                       (lambda ()
                         (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                           (progn (ggtags-mode 1)
                                  (evil-define-key 'normal (current-local-map) (kbd "M-.") 'ggtags-find-tag-dwim))
                           )))
  :bind (:map ggtags-mode-map
              ("C-c g s" . ggtags-find-other-symbol)
              ("C-c g h" . ggtags-view-tag-history)
              ("C-c g r" . ggtags-find-reference)
              ("C-c g f" . ggtags-find-file)
              ("C-c g c" . ggtags-create-tags)
              ("C-c g u" . ggtags-update-tags)

              ("M-," . pop-tag-mark)
              ("C-M-," . ggtags-find-tag-continue))

  ;; (define-key evil-normal-state-map (kbd "M-.") 'ggtags-find-tag-dwim)
  ;; (define-key evil-normal-state-map (kbd "M-.") nil)


  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Quit help windows
;;----------------------------

(defvar my/help-window-names
  '(
    ;; Ubiquitous help buffers
    "*Help*"
    "*Apropos*"
    "*Messages*"
    "*Completions*"
    ;; Other general buffers
    "*Command History*"
    "*Compile-Log*"
    "*disabled command*")
  "Names of buffers that `my/quit-help-windows' should quit.")

(defun my/quit-help-windows (&optional kill frame)
  "Quit all windows with help-like buffers.

Call `quit-windows-on' for every buffer named in
`my/help-windows-name'.  The optional parameters KILL and FRAME
are just as in `quit-windows-on', except FRAME defaults to t (so
that only windows on the selected frame are considered).

Note that a nil value for FRAME cannot be distinguished from an
omitted parameter and will be ignored; use some other value if
you want to quit windows on all frames."
  (interactive)
  (let ((frame (or frame t)))
    (dolist (name my/help-window-names)
      (ignore-errors
        ;; (delete-windows-on name kill frame)))))
        (delete-windows-on name frame)))))

(with-eval-after-load "evil" 
  (define-key evil-window-map "r" 'my/quit-help-windows)
  (define-key evil-window-map (kbd "C-r") 'my/quit-help-windows))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Treemacs
;;----------------------------
(use-package treemacs
  :bind (("<f8>" . treemacs)
         ("<M-f8>" . treemacs-add-and-display-current-project)
         :map treemacs-mode-map
         ([mouse-1] . treemacs-single-click-expand-action))
  :config
  (use-package treemacs-evil)
  (use-package treemacs-projectile)
  (use-package treemacs-magit)

  (defun treemacs-file-tracked-p (file git-info)
    "Determined if FILE is tracked by git by means of GIT-INFO."
    (declare (side-effect-free t))
    (not (string= "?" (ht-get git-info file))))

  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)
  ;; (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-file-tracked-p)

  ;; (treemacs-tag-follow-mode t)
  )

;; I think this is techinically separate
(use-package treemacs-icons-dired)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Org-mode stuff
;;------------------------------------------------
(use-package org
  :init (require 'org-agenda)
         (require 'org-habit)
  :demand t

  :hook (
         (org-mode . auto-fill-mode)
         (org-mode . org-bullets-mode)
         ;; The process-connection-type thing is for xdg-open to work.
         (org-mode . (lambda () (setq tab-width 2) (setq-local process-connection-type nil)))
         )

  :custom ((org-tags-column -100)
           (org-agenda-tags-column -100)
           (org-return-follows-link t)
           (org-log-done 'time)
           (org-directory "~/Dropbox/org")
           (org-default-notes-file (concat org-directory "/notes.org"))
           (org-habit-graph-column 70)
           (org-habit-show-habits-only-for-today nil)
           (org-refile-targets '((org-agenda-files :maxlevel . 3)))
           (org-refile-use-outline-path t)
           (org-outline-path-complete-in-steps nil)
           (org-enforce-todo-dependencies t)
           (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
           (org-deadline-warning-days 5)
           (org-stuck-projects '("TODO={.+}/-DONE" ("CANCELLED") nil "SCHEDULED:\\|DEADLINE:"))
           (org-agenda-skip-deadline-if-done t)
           (org-agenda-skip-scheduled-if-done t)
           (org-clock-out-when-done '("WAITING" "DONE" "CANCELLED"))
           ;; (org-insert-heading-respect-content t)
           ;; TODO: make the tasks thing a bit more automatic.
           (org-capture-templates
            '(("c" "Coding todo entry" entry
               (file+headline "" "Coding")
               "* TODO %i%?    :%f:\n\t%\i\n\t%u\n\t%a")
              ("t" "General task" entry
               (file+headline "" "Tasks")
               "* TODO %i%?    :%f:\n\t%\i\n\t%u\n\t%a")
              ("z" "Miscellaneous" checkitem
               (file+headline "" "Misc")
                                        ;(file "")
                                        ;"- [ ] %i%?\n\t%u"
               )
              ("f" "Cookbook" entry (file "~/Dropbox/org/cookbook.org")
               "%(org-chef-get-recipe-from-url)"
               :empty-lines 1)
              ("g" "Manual Cookbook" entry (file "~/Dropbox/org/cookbook.org")
               "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")))
           (org-confirm-babel-evaluate nil)
           (org-startup-with-inline-images t)
           (org-startup-with-latex-preview t)
           (org-agenda-restore-windows-after-quit t)
           (org-agenda-window-setup 'only-window)
           (org-src-window-setup 'other-window)
           (org-agenda-category-icon-alist `(("home" ,(list (all-the-icons-faicon "home" :height 1.0)) nil nil)
                                             ("notes" ,(list (all-the-icons-faicon "calculator" :height 1.0)) nil nil)
                                             ("travel" ,(list (all-the-icons-faicon "plane" :height 1.25)) nil nil)
                                             ("" ,(list (all-the-icons-faicon "question-circle" :height 1.25)) nil nil)))
           (org-agenda-scheduled-leaders '("Sch: " "%2dx: "))
           (org-agenda-deadline-leaders '("Ded: " "%2dd: " "%2d d. ago: "))
           (org-agenda-prefix-format '((agenda . "%i %?-12t% s%-12(let* ((rawstr (car (last (org-get-outline-path)))) (str (if (> (length rawstr) 10) (substring rawstr 0 9) rawstr))) (concat \"[\" str \"]\")))")
                                       (todo . " %i %-12:c")
                                       (tags . " %i %-12:c")
                                       (search . " %i %-12:c")))
           )

  :config
  (add-to-list 'recentf-exclude "notes.org")
  (add-to-list 'recentf-exclude "home.org")
  (add-to-list 'recentf-exclude "init.el")


  :bind (("<f7>" . danny-orgmode)
         :map danny-orgmode
         ("<f7>" . danny-open-orgfile)
         ("l" . org-store-link)
         ("a" . org-agenda)
         ("c" . org-capture)
         ("b" . org-iswitchb)
         ("j" . org-clock-goto)
         ("o" . org-clock-out)
         ("i" . org-clock-in-last)
         ("r" . remember)
         ("R" . remember-notes)
         ("m" . outshine-imenu)
         :map org-mode-map
         ("<C-M-return>" . org-insert-heading)
         ("C-4" . org-archive-subtree)
         ("<return>" . org-return-indent)
         ("M-a" . nil)
         :map org-babel-map
         ("C-c" . org-babel-hide-result-toggle)
         :map org-agenda-mode-map
         ("M-S-<left>" . (lambda () (interactive) (org-agenda-date-earlier 7)))
         ("M-S-<right>" . (lambda () (interactive) (org-agenda-date-later 7)))

         ("H" . (lambda () (interactive) (org-agenda-date-earlier 1)))
         ("L" . (lambda () (interactive) (org-agenda-date-later 1)))
         ("M-H" . (lambda () (interactive) (org-agenda-date-earlier 7)))
         ("M-L" . (lambda () (interactive) (org-agenda-date-later 7)))
         ("K" . (lambda () (interactive) (org-agenda-priority-up)))
         ("J" . (lambda () (interactive) (org-agenda-priority-down)))
         ("s" . org-agenda-schedule))


  :custom-face
  (org-level-1 ((t (:height 1.5 :family "Liberation Mono"))))
  (org-level-2 ((t (:height 1.2 :family "Liberation Mono"))))
  (org-agenda-dimmed-todo-face ((t (:foreground "#5fafd7" :background "#500"))))

  (org-code ((t (:background "#500000"))))

  (org-block-begin-line ((t (:background "#3a3a3a" :foreground "#777"))))
  (org-block-end-line ((t (:background "#3a3a3a" :foreground "black"))))

  :config
  (defface my/org-results-keyword
    '((t :inherit org-code :foreground "black"))
    "asdf")

  (use-package org-fancy-priorities
    :diminish org-fancy-priorities-mode
    :config
    (add-hook 'org-agenda-mode-hook 'org-fancy-priorities-mode)
    (add-hook 'org-mode-hook 'org-fancy-priorities-mode)
    (setq org-fancy-priorities-list
          '((?A . "!") (?B . "") (?C . "") (?D . "")
            (?1 . "⚡") (?2 . "⮬") (?3 . "⮮") (?4 . "")
            (?I . "Important"))))

  ;; Stop org from ignoring buffer directions
  (advice-add 'org-switch-to-buffer-other-window :override 'switch-to-buffer-other-window)

  (defun my/find-RESULTS-END (limit)
    ""
    ;; (message "Did my find original with limit = %d" limit)
    (when (search-forward-regexp ":END:" limit t)
      (let ((end-match-data (match-data))
            match-data-temp)
        (when (search-backward "#+RESULTS:" nil t)
          (let ((results-match-data (match-data)))
            (search-forward-regexp ":END:" nil t)
            (when (equal end-match-data (match-data))
              ;; (set-match-data (list (nth 0 results-match-data) (nth 1 end-match-data)))
              t))))))

  (font-lock-add-keywords 'org-mode '((my/find-RESULTS-END (0 'my/org-results-keyword t))) t)
  (font-lock-add-keywords 'org-mode `((,(regexp-quote "#+RESULTS:") (0 'my/org-results-keyword t))) t)
  (font-lock-add-keywords 'org-mode `((,(org-re-property "RESULTS" nil t) (0 'my/org-results-keyword t))) t)

  ;; TODO: Make disabled (with :eval no) source blocks show in a different colour

  (define-prefix-command 'danny-orgmode)

  (evil-define-key 'insert org-mode-map (kbd "<shift> <tab>") 'org-indent-item)
  (evil-define-key 'insert org-mode-map (kbd "<S-tab>") 'org-indent-item)
  (evil-define-key 'insert org-mode-map (kbd "<S-iso-lefttab>") 'org-indent-item)
  (evil-define-key 'insert org-mode-map (kbd "<backtab>") 'org-indent-item)


  (setq org-modules (append org-modules '(org-habit org-mouse)))

  (danny-add-prettify-greek 'org-mode-hook)
  (add-hook 'org-mode-hook (lambda ()
                             "Beautify Org Checkbox Symbol"
                             (push '("[ ]" .  "☐") prettify-symbols-alist)
                             (push '("[X]" . "☑" ) prettify-symbols-alist)
                             (push '("[-]" . "❍" ) prettify-symbols-alist)
                             (prettify-symbols-mode)))

  (defface org-checkbox-done-text
    '((t (:foreground "#71696A" :strike-through t)))
    "Face for the text part of a checked org-mode checkbox.")
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-checkbox-done-text prepend))
   'append)

  (require 'ox-publish)
  (require 'ox-md)

  (defun danny-open-orgfile
      (&optional arg)
    "Open the default org file. If a prefix is supplied, open the org file in another window."
    (interactive "p")
                                        ;(message "%s" arg)
    (if (and arg (> arg 1))
        (find-file-other-window org-default-notes-file)
      (find-file org-default-notes-file)))


  (my/evil-add-bindings org-agenda-mode-map)
  (add-to-list 'org-agenda-custom-commands '("d" "Day+Stuck" ((agenda "" '(org-agenda-span 'day))
                                                              (stuck))))

  (add-hook 'org-agenda-after-show-hook 'org-reveal)
  (add-hook 'org-agenda-after-show-hook 'org-show-subtree)

  (use-package evil-org)
  (use-package org-bullets)
  (use-package org-chef)
  (use-package htmlize
    :custom
    (org-html-htmlize-output-type 'css))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t))) ; this line activates ditaa

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t))) ; this line activates dot
  
  ;; ob-jupyter requires ob-python for some things as defaults.
  (require 'ob-python)
  (use-package jupyter
    :custom (org-babel-default-header-args:jupyter-julia '((:exports . "both")
                                                           (:results . "value verbatim drawer")
                                                           (:session . "defaultdanny")
                                                           (:async . "yes")
                                                           ;; (:kernel . "julia-1.1_pre")
                                                           ;; (:kernel . "julia-1.3")
                                                           (:kernel . "julia-1.4-quick")
                                                           (:eval . "never-export")))
    :config
    (require 'ob-jupyter)
    (org-babel-do-load-languages 'org-babel-load-languages '((jupyter . t)))
    ;; (setq-default org-babel-default-header-args:jupyter-julia '((:session . "juliasession")))
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)


    (add-hook 'org-mode (lambda () (julia-repl--setup-compilation-mode (current-buffer) nil)
                          (compilation-shell-minor-mode)))

    (defvar my-org-src-mode-map (make-sparse-keymap))
    (define-minor-mode my-org-src-mode
      :init-value nil
      :keymap my-org-src-mode-map)
    (add-hook 'org-src-mode-hook 'my-org-src-mode)
    (define-key my-org-src-mode-map (kbd "C-c C-c") 'jupyter-eval-buffer)
    ;; (evil-define-key 'insert 'jupyter-org-interaction-mode-map (kbd "M-i") (lambda () (interactive) (insert-tab)))
    (evil-define-key 'insert jupyter-org-interaction-mode-map (kbd "M-i") nil)

    (defvar my-org-block-mode-map (make-sparse-keymap))
    (define-minor-mode my-org-block-mode
      :keymap my-org-block-mode-map)
    (evil-define-key '(insert normal visual) my-org-block-mode-map (kbd "C-s C-r") 'org-reveal
																   (kbd "C-s C-a") 'outline-show-all
																   (kbd "C-s C-s") 'org-show-subtree
																   (kbd "C-s C-c") 'org-show-children
																   (kbd "C-s C-e") 'org-show-entry
																   (kbd "C-s C-b") 'outline-show-branches)
    (add-hook 'org-mode 'my-org-block-mode)

    (defun my-org-execute-and-next ()
      (interactive)
      (let* ((my-org-block-mode nil)
             (key (this-single-command-keys))
             (binding (key-binding key t)))
        (org-babel-execute-src-block)
        ;; Unfortunately, regular next-src-block always hides other src blocks
        (cl-letf (((symbol-function 'org-show-context) (lambda (&optional key) nil)))
          (org-babel-next-src-block))))

    ;; TODO in the future turn this into a function
    ;; (org-babel-map-src-blocks nil (org-babel-remove-result))
    
    (define-key-with-fallback my-org-block-mode-map (kbd "M-RET") (my-org-execute-and-next) (org-in-src-block-p))
    (define-key-with-fallback my-org-block-mode-map (kbd "<M-enter>") (my-org-execute-and-next) (org-in-src-block-p))
    (define-key my-org-block-mode-map (kbd "C-c C-j") 'jupyter-repl-restart-kernel)

    (evil-define-key '(normal visual motion) org-mode-map ")" 'org-babel-next-src-block)
    (evil-define-key '(normal visual motion) org-mode-map "(" 'org-babel-previous-src-block)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Latex stuff
;;----------------------------
;; Annoying theme stuff
(defvar my/in-latex-mode nil)
(use-package tex-mode
  :demand t
  :hook ((LaTeX-mode . auto-fill-mode)
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . latex-preview-pane-mode)
         (LaTeX-mode . (lambda () (progn
                        ;; lambda () (load-theme 'material-light)
                         (add-hook 'after-save-hook 'preview-buffer nil t)
                         (setq-local company-idle-delay 2.0)
                         (setq-local my/in-latex-mode t))))
         (LaTeX-mode . company-auctex-init)
         (doc-view-mode . (lambda () (setq-local display-line-numbers nil)))
         (doc-view-mode . doc-view-fit-width-to-window))

  :custom
  ((latex-preview-pane-use-frame t)
   (doc-view-resolution 300)
   (preview-gs-options '("-q" "-dNOSAFER" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4")))

  :config
  ;; (when (string= (system-name) "pengix")
  ;;   (setq-default preview-orientation 'below))

  (use-package latex-math-preview)
  (use-package latex-pretty-symbols)
  (use-package latex-preview-pane)
  ;; (use-package material-theme)
  (use-package company-auctex)

  (TeX-global-PDF-mode nil)

                                        ; Hopefully fix being able to read font in error regions
  ;; (add-hook 'LaTeX-mode-hook (lambda () (set-face-attribute 'preview-face nil :inverse-video t)))

  (unbind-key "k" image-mode-map)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Ediff stuff
;;----------------------------
(setq-default ediff-diff-options "-w")
(setq-default ediff-split-window-function 'split-window-horizontally)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * which-key mode
;;------------------------------------------------
(use-package which-key
  :diminish which-key-mode
  :custom (which-key-idle-delay 0.2)
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Outshine/outorg/navi-mode stuff
;;----------------------------

(use-package outshine
  :hook ((prog-mode . outline-minor-mode)
         (outline-minor-mode . (lambda () (progn
                                            (outshine-mode)
                                            (setq-local outshine-imenu-preliminary-generic-expression
                                                        `((nil ,(concat (outshine-calc-outline-regexp) "\\(.*$\\)") 1)))))))
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
  ;; Need this to help with outshine-imenu
  ;; (add-hook 'outline-minor-mode-hook (lambda () (setq-local outshine-imenu-preliminary-generic-expression
  ;;                `((nil ,(concat (message "%s" (outshine-calc-outline-regexp)) "\\(.*$\\)") 1)))))

  (defvaralias 'outline-promotion-headings 'outshine-promotion-headings)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Polymode
;;------------------------------

(use-package poly-markdown)

(use-package poly-org
  :config
  (set-slot-value poly-org-innermode 'adjust-face 10)
  )

(add-hook 'markdown-mode-hook #'auto-fill-mode)
(add-hook 'markdown-mode-hook (lambda () (setq-local auto-fill-function #'markdown-fill-paragraph)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * LSP
;;----------------------------

(setq lsp-keymap-prefix "s-i")

(setq lsp-julia-package-dir nil)
(add-to-list 'load-path "~/.emacs.d/mypackages/lsp-julia/")
(require 'lsp-julia)

(use-package lsp-mode
  :hook (
         ;;(julia-mode . my/check-julia-lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-ui-mode))

  :custom ((lsp-ui-doc-position 'bottom)
           (lsp-ui-sideline-show-hover t)
           (lsp-ui-sideline-ignore-duplicate t)
           (lsp-ui-doc-max-height 10)
           (lsp-ui-sideline-update-mode 'line))

  :custom-face
  (markdown-code-face ((t (:inherit default))))
  (lsp-ui-sideline-global ((t (:background "navy"))))


  :config
  (use-package lsp-ui :commands lsp-ui-mode)
  (use-package company-lsp :commands company-lsp)
  ;; if you are helm user
  (use-package helm-lsp :commands helm-lsp-workspace-symbol)
  (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

  ;; optionally if you want to use debugger
  (use-package dap-mode)
  ;; (use-package dap-LANGUAGE) to load the dap adapter for your language


  (defun my/check-julia-lsp ()
    "Make sure that the current buffer is a .jl file and not jupyter etc..."
    (when (and (s-ends-with-p ".jl" (buffer-file-name))
               (not (string-equal "/home/pengwyn" (lsp--suggest-project-root))))
      (lsp)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Julia
;;----------------------------
(setq-default julia-mode-use-poly t)

(defvar my/org-src-block-override-map (make-sparse-keymap "Just for running src blocks"))

(define-key my/org-src-block-override-map (kbd "C-c C-c") 'my/base-buffer-execute-src-block)
(define-key my/org-src-block-override-map (kbd "M-RET") 'my/base-buffer-execute-src-block-and-next)
;; This doesn't work - probably because the override doesn't lock in with evil emulation mode.
(evil-define-key '(normal visual motion) my/org-src-block-override-map ")" 'my/base-buffer-next-block)
(evil-define-key '(normal visual motion) my/org-src-block-override-map "(" 'my/base-buffer-previous-block)

(define-polymode poly-org-mode
  :hostmode 'poly-org-hostmode
  :innermodes '(poly-org-innermode)
  ;; :keymap `((,(kbd "C-c C-c") . my/test-asdf))
  (setq-local org-src-fontify-natively nil)
  (make-local-variable 'polymode-move-these-minor-modes-from-old-buffer)
  (push 'org-indent-mode polymode-move-these-minor-modes-from-old-buffer)
  (add-to-list (make-local-variable 'minor-mode-overriding-map-alist) `(julia-repl-mode . ,my/org-src-block-override-map))
)

(defun my/base-buffer-execute-src-block ()
  (interactive)
  (with-current-buffer (buffer-base-buffer)
    (org-babel-execute-src-block-maybe)))
(defun my/base-buffer-execute-src-block-and-next ()
  (interactive)
  (with-current-buffer (buffer-base-buffer)
    (my-org-execute-and-next)))

(defun my/base-buffer-next-block ()
  (interactive)
  (with-current-buffer (buffer-base-buffer)
  (org-babel-next-src-block)))
(defun my/base-buffer-previous-block ()
  (interactive)
  (with-current-buffer (buffer-base-buffer)
  (org-babel-previous-src-block)))


(add-to-list 'load-path "~/.emacs.d/mypackages/julia-emacs/")
(require 'julia-mode)

(use-package julia-mode
  :ensure nil
  :hook (
         (julia-mode . ggtags-mode)
         (julia-mode . julia-math-mode)
         (julia-mode . julia-repl-mode)
         (julia-repl . julia-repl-use-emacsclient)
         (julia-mode . highlight-symbol-mode))
  
  :config
  (add-hook 'julia-mode-hook (lambda ()
                               (make-local-variable 'ggtags-process-environment)
                               (setq ggtags-process-environment
                                     (setenv-internal ggtags-process-environment "GTAGSLABEL" "julia" t))
                               ))


  :custom
  (julia-max-block-lookback 50000)
  (julia-repl-switches "-i --color=yes --sysimage=/home/pengwyn/.julia/config/ijulia_sysimage.so")

  :bind (:map julia-mode-map
         ("<f5>" . julia-repl)
         ("C-c <f5>" . my/julia-set-bp)
         ;; (")" . my/evil-forward-block-end)
         ;; ("(" . my/evil-backward-block-begin)
         :map julia-repl-mode-map
         ("C-c C-b" . my/julia-repl-send-buffer))

  :config

  (require 'company-gtags)
  (setq company-gtags-modes (append company-gtags-modes '(julia-mode-prog-mode)))
  (add-hook 'julia-mode-hook (lambda () (setq-local company-backends '( company-gtags company-dabbrev-code))))

  (defun my/julia-set-bp ()
    (interactive)
    "Send a breakpoint set command to the julia-repl"
    (let ((filename (buffer-file-name))
          (lineno (int-to-string (line-number-at-pos nil t))))
      (message filename)
      (message "%s" lineno)
      (julia-repl--send-string (concat "breakpoint(\"" filename "\"," lineno ")")))
    )

  (use-package julia-repl)

  (defun julia-forward-block (n)
    (let ((n n))
      (while (> n 0)
        (if (julia-at-end-keyword)
            (julia-start-of-next-block)
          (julia-end-of-this-block))
        (setq n (1- n)))
      (while (< n 0)
        (if (julia-at-start-keyword)
            (julia-end-of-last-block)
          (julia-start-of-this-block))
        (setq n (1+ n)))
       ))

  (defvar julia-block 'danny-julia-block)
  (put julia-block 'forward-op 'julia-forward-block)
  (evil-define-motion my/evil-forward-block-end (count)
    :jump t
    :type exclusive
    (evil-forward-end julia-block count))
  (evil-define-motion my/evil-backward-block-begin (count)
    :jump t
    :type exclusive
    (evil-backward-beginning julia-block count))
  (evil-define-key '(normal visual motion) julia-mode-map ")" 'my/evil-forward-block-end)
  (evil-define-key '(normal visual motion) julia-mode-map "(" 'my/evil-backward-block-begin)

  (evil-define-key '(normal visual motion) julia-mode-map "{" 'julia-start-of-this-block)
  (evil-define-key '(normal visual motion) julia-mode-map "}" 'julia-end-of-this-block)

;; TODO: Write a "move forward/backward block"
;; This should replace a "sentence" move in evil.
;; Even though I have this above, it doesn't work well to skip over inner regions.

  ;; TODO: Write a thing to hook into the "show current function at top" like semantic does.

  (defun my/julia-repl-send-buffer (arg)
    "Send the contents of the current buffer to the Julia
     REPL.

     (No! This was bad for script-like files) Uses includet(...) instead of include(...).

     With an arg, first change the working directory to the
     location of the file."

    (interactive "P")
    (let* ((file buffer-file-name))
      (when (and file (buffer-modified-p))
        (if (y-or-n-p "Buffer modified, save? ")
            (save-buffer)
          (setq file nil)))
      (when arg
        (julia-repl--send-string (concat "cd(\"" (file-truename default-directory) "\")")))
      (julia-repl--send-string
       (if file
           ;; (concat "includet(\"" file "\")")
         (concat "include(\"" file "\")")
         (buffer-substring-no-properties (point-min) (point-max))))))

  (define-key LaTeX-math-keymap "'" 'LaTeX-math-prime)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Javascript
;;----------------------------
(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.js\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook 'flymake-eslint-enable)
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

  (use-package fontawesome)
  (add-hook 'web-mode-hook (lambda () 
                             (setq-local prettify-symbols-alist (mapcar (lambda (fa-cons)
                                                                    (cons (concat "fa-" (car fa-cons))
                                                                          (aref (cdr fa-cons) 0)))
                                                                  fontawesome-alist))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Projectile
;;----------------------------
(use-package projectile
  :demand
  :custom
  ((projectile-mode-line-prefix " ")
   (projectile-switch-project-action #'helm-projectile-find-file))
  :config
  (projectile-mode)
  (use-package helm-projectile)
  (use-package projectile-direnv
    :hook (projectile-mode . projectile-direnv-export-variables)
    )
  (use-package projectile-variable)

  (defun my/open-projectile-or-current-directory ()
    (interactive)
    (let ((directory (or (projectile-project-root)
                        (file-name-directory buffer-file-name))))
      (browse-url-xdg-open (file-truename directory))))
  
  (define-prefix-command 'danny-projectile)

  (defun my/projectile-compile (arg)
    (interactive "P")
    (projectile-save-project-buffers)
    (let ((compilation-save-buffers-predicate 'ignore)
          (compilation-read-command nil))
      (projectile-compile-project arg)))

  :bind (("<f9>" . danny-projectile)
         :map danny-projectile
         ("<f9>" . 'helm-projectile-switch-project)
         ("f" . 'helm-projectile-find-file-dwim)
         ;; ("p" . 'org-publish-current-project)
         ;; ("p" . 'projectile-compile-project)
         ("p" . 'my/projectile-compile)
         ("a" . 'helm-projectile-ag)
         ("d" . 'projectile-dired)
         ("x" . 'my/open-projectile-or-current-directory)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Term mode
;;----------------------------

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Frames over windows
;;----------------------------

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Visit files with line numbers
;;--------------------------------
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Key bindings
;;----------------------------

(define-prefix-command 'danny-completions)

(evil-define-key '(insert normal) 'global (kbd "C-k") 'danny-completions)
(define-key danny-completions (kbd "C-l") 'evil-complete-previous-line)
(define-key danny-completions (kbd "C-o") 'helm-occur)
(define-key danny-completions (kbd "C-k") 'helm-resume)
(define-key danny-completions (kbd "C-a") 'helm-do-grep-ag)
;; (define-key danny-completions (kbd "C-d") 'ggtags-find-definition)
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
;; * THEME
;;----------------------------

(use-package unicode-fonts
  :custom
  (unicode-fonts-fallback-font-list '("Symbola" "Quivira" "DejaVu Math Tex Gyre"))
  :config
  (unicode-fonts-setup)
  )


(defvar my/loaded-theme nil)

(require 'moe-theme)
(defun apply-color-theme (frame)
  "Apply color theme to a frame based on whether its a 'real'
   window or a console window."
  (select-frame frame)
  ;; (unless my/in-latex-mode
  ;;   ;; (set-background-color "black")
  ;;   )
  (when (and (>= (length (frame-list)) 2) (not my/loaded-theme) (display-graphic-p))
  ;; (when t
    ;; (load-theme 'moe-dark t)
    ;; (enable-theme 'moe-dark)
    (let (moe-theme-revert-theme)
      (moe-dark))
    ;; (custom-theme-set-faces 'moe-dark '(default ((t (:background "#000000" :foreground "#c6c6c6")))))
    ;; (custom-theme-set-faces 'moe-dark '(compilation-error ((t (:foreground "#333" :background "#faa" :weight bold)))))
    ;; (set-face-font 'default "Gohu GohuFont-14")
    (set-face-font 'default "Mononoki Nerd Font-10")
    (setq my/loaded-theme t)
    ))


(load-theme 'moe-dark t nil)
(custom-theme-set-faces 'moe-dark '(default ((t (:background "#000000" :foreground "#c6c6c6")))))
(custom-theme-set-faces 'moe-dark '(compilation-error ((t (:foreground "#333" :background "#faa" :weight bold)))))

(if (daemonp)
    (progn
      (add-hook 'after-make-frame-functions 'apply-color-theme)
      (add-to-list 'default-frame-alist '(fullscreen . maximized))
      )
  ;; else
  (apply-color-theme (selected-frame))
  )


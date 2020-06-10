
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Short modes
;;----------------------------

;; ** Essentials

(use-package which-key
  :diminish which-key-mode
  :demand t
  :bind (("<M-f12>" . which-key-show-top-level))
  :custom (which-key-idle-delay 0.2)
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Evil
;;----------------------------
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  
  :demand t

  :custom ((evil-cross-line t)
           (evil-visual-x-select-timeout 999)
           (evil-symbol-word-search 'symbol)
           ;; (evil-want-fine-undo t)
           )

  :bind (:map evil-window-map
         ("C-l" . evil-window-right)
         ("C-h" . evil-window-left)
         ("C-k" . evil-window-up)
         ("C-j" . evil-window-down)
         ("C-w" . evil-window-mru)

         :map evil-visual-state-map
         (">" . my/evil-block-shift-right)
         ("<" . my/evil-block-shift))

  :config
  (evil-mode 1)

  (define-key evil-motion-state-map [down-mouse-1] nil)
  
  (defalias #'forward-evil-word #'forward-evil-symbol)

  (defun my/evil-block-shift (&optional right)
    (interactive)
    ;; ensure mark is less than point
    (when (> (mark) (point)) 
      (exchange-point-and-mark))
    (evil-normal-state)
    (if right
      (evil-shift-right (mark) (point))
      (evil-shift-left (mark) (point)))
	(evil-visual-restore))
    (defun my/evil-block-shift-right ()
    (interactive)
    (call-interactive my/evil-block-shift t))


  (use-package evil-collection
    :config
    (evil-collection-init))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Helm
;;----------------------------
(use-package helm
  :demand t
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
  ))

;; Kind of helm related
(use-package tramp
  :config
  (setenv "SHELL" "/bin/bash"))



;;;;;;;;;;;;;;;;;
;; * Other stuff

(evil-define-key 'visual lisp-mode-shared-map (kbd "C-M-x") 'eval-region)

(savehist-mode t)
(save-place-mode t)
(show-paren-mode t)
(global-auto-revert-mode t)

(use-package ibuffer
  :bind (:map ctl-x-map
         ("C-b" . ibuffer)))

(use-package highlight-symbol
  :config
  (highlight-symbol-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package undo-tree
  :custom (undo-tree-enable-undo-in-region nil))


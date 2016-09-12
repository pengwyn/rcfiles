;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;(add-to-list 'package-archives
;             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
	org
    relative-line-numbers
	rainbow-delimiters
    material-theme
    elpy
    ein
    evil
    evil-surround
    evil-numbers
    evil-visualstar
    evil-exchange
    evil-search-highlight-persist
	evil-magit
    magit
	sr-speedbar
	powerline
	powerline-evil
    helm
    ggtags
	company-quickhelp))


(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

(elpy-enable)
(delete 'elpy-module-highlight-indentation elpy-modules)
(delete 'elpy-module-flymake elpy-modules)
(setq elpy-rpc-python-command "python2")
(elpy-use-ipython "ipython2")

(require 'ein)
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'alect-black t) ;; load material theme
;(global-linum-mode t) ;; enable line numbers globally
(global-relative-line-numbers-mode)

;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-margin 3)

(set-frame-font "GohuFont-11")

(global-auto-revert-mode t)

(global-set-key (kbd "<f5>") 'compile)
(setq-default compilation-read-command nil)
(setq-default compile-command "make")
(global-set-key (kbd "C-j") 'next-error)

(require 'magit)
(global-set-key (kbd "<f6>") 'magit-status)


(require 'rainbow-delimiters)
;; (rainbow-delimiters-mode t)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(setq python-shell-interpreter "ipython2"
    python-shell-interpreter-args "--simple-prompt -i")

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


(require 'evil)
(evil-mode 1)

(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-visual-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)

(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
(define-key evil-visual-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

(define-key evil-normal-state-map (kbd "C-w C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-w C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-w C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-w C-j") 'evil-window-down)

;; (define-key evil-insert-state-map (kbd "C-a") 'evil-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'evil-end-of-line)

(define-key evil-visual-state-map ">" (lambda ()
    (interactive)
    ; ensure mark is less than point
    (when (> (mark) (point)) 
        (exchange-point-and-mark)
    )
    (evil-normal-state)
    (evil-shift-right (mark) (point))
    (evil-visual-restore) ; re-select last visual-mode selection
))

(define-key evil-visual-state-map "<" (lambda ()
    (interactive)
    ; ensure mark is less than point
    (when (> (mark) (point)) 
        (exchange-point-and-mark)
    )
    (evil-normal-state)
    (evil-shift-left (mark) (point))
    (evil-visual-restore) ; re-select last visual-mode selection
))

(setq-default evil-symbol-word-search 'symbol)

(defun recenter-top-bottom-with-clear ()
  "Do the normal recenter and redraw the screen."
  (interactive)
  (recenter-top-bottom)
  (evil-search-highlight-persist-remove-all))

(define-key evil-normal-state-map (kbd "C-x C-<space>") 'recenter-top-bottom-with-clear)
(define-key evil-insert-state-map (kbd "C-x C-<space>") 'recenter-top-bottom-with-clear)
(define-key evil-normal-state-map (kbd "C-x <space>") 'recenter-top-bottom-with-clear)
(define-key evil-insert-state-map (kbd "C-x <space>") 'recenter-top-bottom-with-clear)

(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))

(require 'evil-magit)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-exchange)
(setq evil-exchange-key (kbd "zx"))
(evil-exchange-install)

(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)
(setq evil-search-highlight-string-min-len 3)



(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(setq-default company-minimum-prefix-length 2)
(setq-default company-idle-delay 0.1)

;(setq-default company-backends '( (:separate company-semantic company-clang company-gtags) company-yasnippet company-capf company-dabbrev))
(setq-default company-backends '( company-clang company-semantic company-gtags company-yasnippet company-capf company-dabbrev))
;(setq-default company-backends '( company-yasnippet))

(setq-default company-dabbrev-time-limit 1.0)

(defun company-yasnippet-or-completion ()
  "Solve company yasnippet conflicts."
  (interactive)
  (let ((yas-fallback-behavior
         (apply 'company-complete-common nil)))
    (yas-expand)))

(add-hook 'company-mode-hook
          (lambda ()
            (substitute-key-definition
             'company-complete-common
             'company-yasnippet-or-completion
             company-active-map)))

;; (add-hook 'elpy-mode-hook
;; 	(lambda () (setcar company-backends '(:separate elpy-company-backend company-yasnippet)))
;; )
(define-key company-mode-map (kbd "C-M-i") 'company-complete)
(define-key company-mode-map (kbd "C-<tab>") 'company-other-backend)

(require 'company-quickhelp)
(company-quickhelp-mode t)


(require 'semantic)
(require 'semantic/db-global)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)
(semantic-mode 1)


(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)
(define-key evil-normal-state-map (kbd "M-o") 'helm-recentf)

(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;(define-key evil-normal-state-map (kbd "M-.") 'ggtags-find-tag-dwim)
(define-key evil-normal-state-map (kbd "M-.") nil)

(setq-default c-default-style "linux"
              c-basic-offset 4
              tab-width 4
              indent-tabs-mode t)

(setq-default split-height-threshold 20
			  split-width-threshold 20)

(require 'powerline)
(powerline-center-evil-theme)


(define-prefix-command 'danny-completions)
(define-key evil-insert-state-map (kbd "C-k") 'danny-completions)
(define-key evil-normal-state-map (kbd "C-k") 'danny-completions)
(define-key danny-completions (kbd "C-l") 'evil-complete-previous-line)
(define-key danny-completions (kbd "C-o") 'helm-occur)
(define-key danny-completions (kbd "C-k") 'helm-resume)

(global-set-key (kbd "C-x f") 'helm-multi-files)


 ;; (defun kill-dired-buffers ()
 ;; 	 (interactive)
 ;; 	 (mapc (lambda (buffer) 
 ;;           (when (eq 'dired-mode (buffer-local-value 'major-mode buffer)) 
 ;;             (kill-buffer buffer))) 
 ;;         (buffer-list)))


(setq
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 ;; gdb-show-main t
 gdb-show-main nil
 )

(require 'sr-speedbar)
;(setq speedbar-show-unknown-files t) ; show all files
(setq speedbar-use-images nil) ; use text for buttons
;(setq sr-speedbar-right-side nil) ; put on left side
(setq speedbar-hide-button-brackets-flag t)
(setq speedbar-tag-hierarchy-method '(speedbar-sort-tag-hierarchy))
(global-set-key (kbd "<f8>") 'sr-speedbar-toggle)

(require 'gud)
(define-key gud-minor-mode-map (kbd "<f9>") 'gud-next)
(add-hook 'gud-mode-hook
		  (lambda () (tool-bar-mode t)))

(eval-after-load 'comint
  '(progn
    (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
    (define-key comint-mode-map (kbd "C-p") 'comint-previous-input)
    (define-key comint-mode-map (kbd "<down>") 'comint-next-input)
    (define-key comint-mode-map (kbd "C-n") 'comint-previous-input)))

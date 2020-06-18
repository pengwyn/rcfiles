;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Packages
;;----------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa stable" . "https://stable.melpa.org/packages/"))

(setq package-archive-priorities
      '(("melpa stable" . 0)
        ("gnu" . -10)
        ("melpa" . 5)))

(package-initialize)

;; ** First time setup
;; When starting from an empty config, install use-package.
(unless (locate-library "use-package")
  (package-refresh-contents)
  (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * use-package
;;----------------------------

(require 'use-package)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package diminish)
(use-package bind-key)
(use-package hydra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * General stuff
;;----------------------------

(setq x-select-enable-clipboard nil)

(setq-default ;; inhibit-startup-message t ;; hide the startup message
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
              ;; display-line-numbers nil
              indent-tabs-mode nil
              tab-width 4
              dired-auto-revert-buffer t
              sentence-end-double-space nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(load "~/.emacs.d/utils.el")

(global-set-key (kbd "<S-down-mouse-1>") 'mouse-save-then-kill)
(global-set-key (kbd "<f12>") 'switch-to-minibuffer-window)
(global-set-key (kbd "<f2>") 'package-list-packages)
(global-set-key (kbd "<f3>") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "<f4>") 'save-buffers-kill-emacs)
(define-key help-map (kbd "C-c") 'customize-group) 
;; Get rid of accidental mouse pastes.
(global-set-key (kbd "<mouse-2>") nil)

(load "~/.emacs.d/init_essentials.el")

(load "~/.emacs.d/init_frames.el")

(load "~/.emacs.d/init_extras.el")

;; (load "~/.emacs.d/init_extras_julia.el")

(load "~/.emacs.d/init_danny.el")

(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Emacs customize
;;----------------------------
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

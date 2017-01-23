;; init.el --- Emacs configuration


(define-obsolete-function-alias 'danny-load-all-packages 'package-install-selected-packages)

;; INSTALL PACKAGES
;; --------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa stable" . "https://stable.melpa.org/packages/"))
;(add-to-list 'package-archives
;             '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(setq package-archive-priorities
	  '(("melpa stable" . 5)
		("gnu" . 0)
		("melpa" . -5)))

(package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs customize stuff automatically added in below
;;---------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "b0ab5c9172ea02fba36b974bbd93bc26e9d26f379c9a29b84903c666a5fde837" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" default)))
 '(fill-column 80)
 '(org-agenda-files (quote ("~/Dropbox/org/notes.org")))
 '(package-selected-packages
   (quote
	(ess prettify-greek flycheck helm-flycheck dim which-key vdiff goto-chg auctex latex-math-preview latex-pretty-symbols latex-preview-pane julia-shell julia-mode sr-speedbar rtags relative-line-numbers rainbow-delimiters powerline-evil material-theme list-processes+ helm-ag ggtags evil-visualstar evil-surround evil-search-highlight-persist evil-numbers evil-magit evil-exchange elpy ein company-quickhelp better-defaults badger-theme alect-themes evil helm magit org powerline)))
 '(preview-auto-cache-preamble t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;; Put this at the start to allow removal of minor modes as we go
(require 'dim)
;; Some common ones
(dim-minor-name 'yas-minor-mode nil 'yasnippet)
(dim-minor-name 'undo-tree-mode nil 'undo-tree)
(dim-minor-name 'abbrev-mode nil 'abbrev)
(dim-minor-name 'auto-fill-function nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python stuff
;;----------------------------
(require 'elpy)
(setq-default eply-remove-modeline_lighter nil)
(delete 'elpy-module-highlight-indentation elpy-modules)
(delete 'elpy-module-flymake elpy-modules)
(setq-default elpy-rpc-python-command "python2")
;(elpy-use-ipython "ipython2 --pylab --profile math")
(elpy-use-ipython "ipython2")
;(setq-default python-shell-interpreter "ipython2"
;    python-shell-interpreter-args "--simple-prompt -i")
(setq-default python-shell-interpreter-args "--simple-prompt -i --pylab --profile math")

;(elpy-enable)

(require 'ein)
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General stuff
;;----------------------------
(setq-default inhibit-startup-message t) ;; hide the startup message
(load-theme 'alect-black t) ;; load material theme
;(global-linum-mode t) ;; enable line numbers globally
(global-relative-line-numbers-mode)

;(setq-default mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq-default mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq-default mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq-default scroll-step 1) ;; keyboard scroll one line at a time
(setq-default scroll-conservatively 101) ;; keyboard scroll one line at a time
(setq-default scroll-margin 3)

(set-frame-font "GohuFont-11")

(setq-default split-height-threshold 20
			  split-width-threshold 20)

(global-auto-revert-mode t)
(dim-minor-name 'auto-revert-mode nil 'autorevert)

(require 'magit)
(global-set-key (kbd "<f6>") 'magit-status)


(require 'rainbow-delimiters)
;; (rainbow-delimiters-mode t)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

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

(setq-default recentf-max-saved-items 1000)

(require 'prettify-greek)

; Make up my own set with \ in front of them.
(defconst danny-prettify-set 
	(let* ((my-greek (append (copy-tree prettify-greek-lower) (copy-tree prettify-greek-upper))))
	  (dolist (item my-greek)
		(setcar item (concat "\\" (car item))))
	  (append my-greek prettify-greek-lower prettify-greek-upper)))

(defun danny-prettify-predicate (start end _match) "Only care about words and not symbols."
	   ;; (not (or (= (char-syntax (char-after end)) ?w)
	   ;; 			(= (char-syntax (char-before start)) ?w))))
	   (not (or (string-match-p "[a-zA-Z]" (string (char-after end)))
				(string-match-p "[a-zA-Z]" (string (char-before start))))))
	   

(defun danny-add-prettify-greek (mode) "Add prettify-greek symbols to mode."
	   (add-hook mode (lambda ()
						(setq
							;prettify-symbols-alist (append prettify-symbols-alist prettify-greek-lower prettify-greek-upper)))))
							prettify-symbols-alist (append prettify-symbols-alist danny-prettify-set))
						(prettify-symbols-mode t)
						(setq prettify-symbols-compose-predicate 'danny-prettify-predicate))))

(setq-default prettify-symbols-unprettify-at-point "right-edge")

;;;;;;;;;;;;;;;
;; Evil stuff

(require 'evil)
(evil-mode 1)

(define-key evil-window-map (kbd "C-l") 'evil-window-right)
(define-key evil-window-map (kbd "C-h") 'evil-window-left)
(define-key evil-window-map (kbd "C-k") 'evil-window-up)
(define-key evil-window-map (kbd "C-j") 'evil-window-down)

(dolist (map '(evil-normal-state-map evil-insert-state-map evil-motion-state-map evil-emacs-state-map))
	(define-key (eval map) (kbd "M-l") 'evil-forward-char)
	(define-key (eval map) (kbd "M-h") 'evil-backward-char)
	(define-key (eval map) (kbd "M-k") 'evil-previous-line)
	(define-key (eval map) (kbd "M-j") 'evil-next-line))

(dolist (map '(evil-motion-state-map evil-emacs-state-map))
	(define-key (eval map) (kbd "C-w") 'evil-window-map)
	(define-key (eval map) (kbd "C-M-r") 'helm-recentf))

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

(defun recenter-top-bottom-with-clear ()
  "Do the normal recenter and redraw the screen."
  (interactive)
  (recenter-top-bottom)
  (evil-search-highlight-persist-remove-all))

(dolist (map '(evil-normal-state-map evil-insert-state-map evil-motion-state-map))
	(define-key (eval map) (kbd "C-c +") 'evil-numbers/inc-at-pt)
	(define-key (eval map) (kbd "C-c -") 'evil-numbers/dec-at-pt)

	(define-key (eval map) (kbd "RET") nil)
	(define-key (eval map) (kbd " ") nil)

 (dolist (key '("C-x C-<space>" "C-x <space>" "C-l"))
   (define-key (eval map) (kbd key) 'recenter-top-bottom-with-clear))
)

;; Don't do this because org-agenda has a lot of different bindings.
;; (require 'org)
;; (evil-set-initial-state 'org-agenda-mode 'motion)

(setq-default evil-symbol-word-search 'symbol)
(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))

(require 'evil-magit)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-exchange)
(setq-default evil-exchange-key (kbd "zx"))
(evil-exchange-install)

(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)
(setq-default evil-search-highlight-string-min-len 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet
;;----------------------------

(require 'yasnippet)
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "C-M-y") 'yas-expand)


;;;;;;;;;;;;;;;;
;; Company stuff

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(setq-default company-minimum-prefix-length 2)
(setq-default company-idle-delay 0.2)

;; Note that company-yasnippet is bad and never returns nil so the other
;; backends can never be used.  It's better to use the yas fallback options.
;(setq-default company-backends '( (:separate company-semantic company-clang company-gtags) company-yasnippet company-capf company-dabbrev))
;(setq-default company-backends '( company-clang company-semantic company-gtags company-yasnippet company-capf company-dabbrev))
;(setq-default company-backends '( company-clang company-semantic company-gtags company-capf company-dabbrev))
(setq-default company-backends '( company-clang company-semantic company-gtags company-capf company-keywords company-dabbrev-code))
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
;(define-key company-mode-map (kbd "C-M-i") 'company-complete)
(define-key company-mode-map (kbd "C-<tab>") 'company-other-backend)

(require 'company-quickhelp)
(company-quickhelp-mode t)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm stuff
;;----------------------------
(require 'helm)
(require 'helm-config)

(dim-minor-name 'helm-mode nil 'helm-mode)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq-default helm-google-suggest-use-curl-p t))

(setq-default helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
			  helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
			  helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
			  helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
			  helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(setq-default helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x f") 'helm-find-files)
(global-set-key (kbd "C-x C-f") 'helm-multi-files)
(setq-default helm-semantic-fuzzy-match t
              helm-imenu-fuzzy-match    t)
;; (define-key evil-motion-state-map (kbd "M-p") 'helm-recentf)

(define-key helm-grep-map (kbd "C-.") 'helm-goto-next-file)
(define-key helm-grep-map (kbd "C-,") 'helm-goto-precedent-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ggtags
;;----------------------------
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C Stuff
;;----------------------------
(require 'cc-mode)

(setq-default c-default-style "linux"
              c-basic-offset 4
              tab-width 4
              indent-tabs-mode t)

(require 'semantic)
(require 'semantic/db-global)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

;; This inteferes with too many other things.
;; (global-semantic-idle-summary-mode 1)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

;(semantic-mode 1)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (semantic-mode 1)
			  (flycheck-mode 1))))

;; (global-set-key (kbd "<f5>") 'compile)
(setq-default compilation-read-command nil)
(setq-default compile-command "make")
;; (global-set-key (kbd "C-j") 'next-error)
(define-key c-mode-map (kbd "<f5>") 'compile)
(define-key c-mode-map (kbd "C-j") 'next-error)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Powerline
;;----------------------------
(require 'powerline)
(powerline-center-evil-theme)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My maps
;;----------------------------
(define-prefix-command 'danny-completions)

(define-key evil-insert-state-map (kbd "C-k") 'danny-completions)
(define-key evil-normal-state-map (kbd "C-k") 'danny-completions)
(define-key danny-completions (kbd "C-l") 'evil-complete-previous-line)
(define-key danny-completions (kbd "C-o") 'helm-occur)
(define-key danny-completions (kbd "C-k") 'helm-resume)
(define-key danny-completions (kbd "C-a") 'helm-do-grep-ag)
(define-key danny-completions (kbd "C-d") 'ggtags-find-definition)
(define-key danny-completions (kbd "C-r") 'ggtags-find-reference)
(define-key danny-completions (kbd "C-s") 'ggtags-find-other-symbol)



;; TODO I want to come back to this and have a binding that closes all *...* windows

 ;; (defun kill-dired-buffers ()
 ;; 	 (interactive)
 ;; 	 (mapc (lambda (buffer) 
 ;;           (when (eq 'dired-mode (buffer-local-value 'major-mode buffer)) 
 ;;             (kill-buffer buffer))) 
 ;;         (buffer-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GUD
;;----------------------------

(setq-default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 ;; gdb-show-main t
 gdb-show-main nil
 )

(require 'sr-speedbar)
;(setq-default speedbar-show-unknown-files t) ; show all files
(setq-default speedbar-use-images nil) ; use text for buttons
;(setq-default sr-speedbar-right-side nil) ; put on left side
(setq-default speedbar-hide-button-brackets-flag t)
(setq-default speedbar-tag-hierarchy-method '(speedbar-sort-tag-hierarchy))
(global-set-key (kbd "<f8>") 'sr-speedbar-toggle)

(require 'gud)
(define-key gud-minor-mode-map (kbd "<f9>") 'gud-next)
(add-hook 'gud-mode-hook
		  (lambda () (tool-bar-mode t)))

(eval-after-load 'comint
  '(progn
	(defun danny-prev-match (n) "" (interactive "p")
							   (if (not (comint-after-pmark-p)) (end-of-buffer))
							   (comint-previous-matching-input-from-input n)
							   (setq this-command 'comint-previous-matching-input-from-input))
	(defun danny-next-match (n) "" (interactive "p")
							   (if (not (comint-after-pmark-p)) (end-of-buffer))
							   (comint-next-matching-input-from-input n)
							   (setq this-command 'comint-next-matching-input-from-input))

    ;(define-key comint-mode-map (kbd "<up>") 'comint-previous-matching-input-from-input)
    ;(define-key comint-mode-map (kbd "C-p") 'comint-previous-matching-input-from-input)
    ;(define-key comint-mode-map (kbd "<down>") 'comint-next-matching-input-from-input)
    ;(define-key comint-mode-map (kbd "C-n") 'comint-next-matching-input-from-input)
    (define-key comint-mode-map (kbd "<up>") 'danny-prev-match)
    (define-key comint-mode-map (kbd "C-p") 'danny-prev-match)
    (define-key comint-mode-map (kbd "<down>") 'danny-next-match)
    (define-key comint-mode-map (kbd "C-n") 'danny-next-match)
	))

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode stuff
;;------------------------------------------------
(setq-default org-tags-column -100
              org-agenda-tags-column -100
              org-return-follows-link t
              org-log-done 'time
              org-directory "~/Dropbox/org"
              org-default-notes-file (concat org-directory "/notes.org")
			  org-habit-graph-column 70
			  org-habit-show-habits-only-for-today nil)

(eval-after-load "org" '(setq-default org-modules (append org-modules '(org-habit org-mouse))))

(danny-add-prettify-greek 'org-mode-hook)


(defun danny-open-orgfile
	(&optional arg)
  "Open the default org file. If a prefix is supplied, open the org file in another window."
  (interactive "p")
	   ;(message "%s" arg)
	   (if (and arg (> arg 1))
		   (find-file-other-window org-default-notes-file)
		   (find-file org-default-notes-file)))
(define-prefix-command 'danny-orgmode)
(global-set-key (kbd "<f7>") 'danny-orgmode)

(define-key danny-orgmode (kbd "<f7>") 'danny-open-orgfile)
(define-key danny-orgmode "l" 'org-store-link)
(define-key danny-orgmode "a" 'org-agenda)
(define-key danny-orgmode "c" 'org-capture)
(define-key danny-orgmode "b" 'org-iswitchb)
(define-key danny-orgmode "j" 'org-clock-goto)
(define-key danny-orgmode "o" 'org-clock-out)
(define-key danny-orgmode "i" 'org-clock-in-last)

;; TODO: make the tasks thing a bit more automatic.
(setq-default org-capture-templates
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
		  )))

(add-hook 'org-mode-hook 'auto-fill-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Latex stuff
;;----------------------------
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'prettify-symbols-mode)
(add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)
(add-hook 'LaTeX-mode-hook (lambda () (load-theme 'material-light)))
(add-hook 'doc-view-mode-hook (lambda () (relative-line-numbers-mode -1)))
(add-hook 'LaTeX-mode-hook (lambda () (add-hook 'after-save-hook 'preview-buffer nil t)))

; Delay the company tooltips
(add-hook 'LaTeX-mode-hook (lambda () (setq company-idle-delay 2.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ediff stuff
;;----------------------------
(setq-default ediff-diff-options "-w")
(setq-default ediff-split-window-function 'split-window-horizontally)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key mode
;;------------------------------------------------
(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-right-bottom)
(dim-minor-name 'which-key-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other stuff
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



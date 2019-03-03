;; * init.el --- Emacs configuration


;; (define-obsolete-function-alias 'danny-load-all-packages 'package-install-selected-packages)

;; INSTALL PACKAGES
;; --------------------------------------
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Emacs customize stuff automatically added in below
;;---------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "b0ab5c9172ea02fba36b974bbd93bc26e9d26f379c9a29b84903c666a5fde837" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" default)))
 '(fill-column 80)
 '(julia-max-block-lookback 50000)
 '(menu-bar-mode nil)
 '(minimap-width-fraction 0.1)
 '(minimap-window-location (quote right))
 '(org-agenda-files (quote ("~/Dropbox/org/notes.org")))
 '(package-selected-packages
   (quote
	(helm-projectile htmlize wgrep-helm evil-avy evil-mc multiple-cursors sublimity julia-mode pkgbuild-mode yaml-mode minimap yasnippet-snippets mmm-mode company-php php-mode projectile projectile-direnv projectile-variable outshine outorg helm-navi navi-mode ess prettify-greek flycheck helm-flycheck dim which-key vdiff goto-chg auctex latex-math-preview latex-pretty-symbols latex-preview-pane julia-shell sr-speedbar rtags relative-line-numbers rainbow-delimiters powerline-evil material-theme list-processes+ helm-ag ggtags evil-visualstar evil-surround evil-search-highlight-persist evil-numbers evil-magit evil-exchange elpy ein company-quickhelp better-defaults badger-theme alect-themes evil helm magit org powerline nlinum nlinum-relative)))
 '(preview-auto-cache-preamble t)
 '(safe-local-variable-values
   (quote
	((org-publish-project-alist
	  ("org-notes" :base-directory "~/Dropbox/Physics/Students/ScatteringSteps/org" :base-extension "org" :publishing-directory "/ssh:scucomp1.anu.edu.au:public_html/ScatteringSteps" :recursive t :publishing-function org-html-publish-to-html :headline-levels 4 :auto-preamble t)
	  ("org-static" :base-directory "~/Dropbox/Physics/Students/ScatteringSteps/org" :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|svg" :publishing-directory "/ssh:scucomp1.anu.edu.au:public_html/ScatteringSteps" :recursive t :publishing-function org-publish-attachment)
	  ("org" :components
	   ("org-notes" "org-static")))
	 (projectile-project-compilation-cmd function org-publish-current-project)
	 (projectile-project-name . "ScatSteps")
	 (org-publish-project-alist
	  ("org-notes" :base-directory "~/Dropbox/Physics/Students/ScatteringSteps/org" :base-extension "org" :publishing-directory "~/Dropbox/Physics/Students/ScatteringSteps/public_html/" :recursive t :publishing-function org-html-publish-to-html :headline-levels 4 :auto-preamble t)
	  ("org-static" :base-directory "~/Dropbox/Physics/Students/ScatteringSteps/org" :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|svg" :publishing-directory "~/Dropbox/Physics/Students/ScatteringSteps/public_html/" :recursive t :publishing-function org-publish-attachment)
	  ("org" :components
	   ("org-notes" "org-static"))))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-font-face ((t (:height 20 :family "DejaVu Sans Mono"))))
 '(preview-face ((t (:inverse-video t)))))


;; Put this at the start to allow removal of minor modes as we go
(require 'dim)
;; Some common ones
(dim-minor-name 'yas-minor-mode nil 'yasnippet)
(dim-minor-name 'undo-tree-mode nil 'undo-tree)
(dim-minor-name 'abbrev-mode nil 'abbrev)
(dim-minor-name 'auto-fill-function nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Python stuff
;;----------------------------
;; (require 'elpy)
;; (setq-default eply-remove-modeline_lighter nil)
;; (delete 'elpy-module-highlight-indentation elpy-modules)
;; (delete 'elpy-module-flymake elpy-modules)
;; (setq-default elpy-rpc-python-command "python2")
;; ;(elpy-use-ipython "ipython2 --pylab --profile math")
;; (elpy-use-ipython "ipython2")
;; ;(setq-default python-shell-interpreter "ipython2"
;; ;    python-shell-interpreter-args "--simple-prompt -i")
;; (setq-default python-shell-interpreter-args "--simple-prompt -i --pylab --profile math")
;; 
;; ;(elpy-enable)
;; 
;; (require 'ein)
;; (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * General stuff
;;----------------------------
(setq-default inhibit-startup-message t) ;; hide the startup message
(global-nlinum-relative-mode)

(setq x-select-enable-clipboard nil)

(when (display-graphic-p)
	(load-theme 'alect-black t) ;; load material theme
	)

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



(setq-default recentf-max-saved-items 1000)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Short modes
;;----------------------------

(require 'magit)

(save-place-mode 1)
(show-paren-mode 1)

(require 'rainbow-delimiters)
;; (rainbow-delimiters-mode t)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'powerline)
(powerline-center-evil-theme)

;; (require 'projectile-direnv)
;; (add-hook 'projectile-mode-hook 'projectile-direnv-export-variables)
(projectile-mode)
(setq projectile-switch-project-action #'helm-projectile-find-file)
(require 'helm-projectile)

(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
(mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Commenting things
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Prettify
;;----------------------------

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


;;;;;;;;;;;;;;;
;; * Evil stuff

(require 'evil)
(evil-mode 1)

(setq evil-cross-line t)
; A cheat to disable copying to x clipboard.
(setq evil-visual-x-select-timeout 999)

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

	;; (define-key (eval map) (kbd "C-n") nil)
	;; (define-key (eval map) (kbd "C-p") nil)

 (dolist (key '("C-x C-<space>" "C-x <space>" "C-l"))
   (define-key (eval map) (kbd key) 'recenter-top-bottom-with-clear))
)

;; Don't do this because org-agenda has a lot of different bindings.
;; (require 'org)
;; (evil-set-initial-state 'org-agenda-mode 'motion)

(setq-default evil-symbol-word-search 'symbol
			  evil-want-fine-undo t)
(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))

(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

; Redefine M-y to copy to clipboard and M-p to paste from clipboard
(evil-define-operator danny-evil-clip-yank (beg end type register yank-handler)
  (evil-yank beg end type ?+ yank-handler))
(evil-define-operator danny-evil-clip-paste (count &optional register yank-handler)
  (interactive "P<x>")
  (evil-paste-after 1 ?+ yank-handler))
(evil-define-key '(normal visual) 'global (kbd "M-y") 'danny-evil-clip-yank)
(evil-define-key '(normal insert) 'global (kbd "M-p") 'danny-evil-clip-paste)

(require 'evil-magit)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-exchange)
(setq-default evil-exchange-key (kbd "zx"))
(evil-exchange-install)

(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)
(setq-default evil-search-highlight-string-min-len 3)

(eval-after-load 'evil-mc '(evil-define-key 'normal evil-mc-key-map "\M-n" nil))
(eval-after-load 'evil-mc '(evil-define-key 'normal evil-mc-key-map "\M-p" nil))

(require 'evil-mc)
(global-evil-mc-mode 1)
(evil-define-key '(normal visual) evil-mc-key-map (kbd "C-p") nil)
(evil-define-key '(normal visual) evil-mc-key-map (kbd "C-n") nil)


(setq-default evil-mc-one-cursor-show-mode-line-text nil)

;(add-to-list 'evil-mc-custom-known-commands '(outshine-self-insert-command . ((:default . evil-mc-execute-default-call-with-count))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Additional commands
;;----------------------------

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

(defun danny-evil-mc-edit-lines (&optional arg)
  "Stolen from multiple cursors"
  (interactive "P")
  (when (not (and mark-active (/= (point) (mark))))
    (error "Mark a set of lines first"))
  (let* ((col (current-column))
         (point-line (line-number-at-pos))
         (mark-line (progn (exchange-point-and-mark) (line-number-at-pos)))
         (direction (if (< point-line mark-line) :up :down)))
    (deactivate-mark)
    (when (and (eq direction :up) (bolp))
      (previous-logical-line 1 nil)
      (move-to-column col))
    ;; Add the cursors
    (while (not (eq (line-number-at-pos) point-line))
      ;; create the cursor
      (evil-mc-make-cursor-here)
      ;; proceed to next
      (if (eq direction :up)
          (previous-logical-line 1 nil)
        (next-logical-line 1 nil))
      (move-to-column col))
    ))

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

  (evil-define-key 'visual global-map "gI" 'evil-mc-insert-vertical-cursors)
  (evil-define-key 'visual global-map "gA" 'evil-mc-append-vertical-cursors)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Yasnippet
;;----------------------------

(require 'yasnippet)
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "C-M-y") 'yas-expand)


;;;;;;;;;;;;;;;;
;; * Company stuff

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq-default company-global-modes '(not shell-mode))

(setq-default company-minimum-prefix-length 2)
(setq-default company-idle-delay 0.2)

;; Note that company-yasnippet is bad and never returns nil so the other
;; backends can never be used.  It's better to use the yas fallback options.
;(setq-default company-backends '( (:separate company-semantic company-clang company-gtags) company-yasnippet company-capf company-dabbrev))
;(setq-default company-backends '( company-clang company-semantic company-gtags company-yasnippet company-capf company-dabbrev))
;(setq-default company-backends '( company-clang company-semantic company-gtags company-capf company-dabbrev))
;(setq-default company-backends '( company-clang company-semantic company-gtags company-capf company-keywords company-dabbrev-code))
;(setq-default company-backends '( (:separate company-clang company-semantic company-gtags company-capf company-keywords) company-dabbrev-code))
;(setq-default company-backends '( (:separate company-clang company-semantic company-gtags company-capf company-keywords) company-dabbrev-code company-dabbrev))
(setq-default company-backends '( company-clang company-semantic company-gtags company-capf company-keywords company-dabbrev-code company-dabbrev))
;(setq-default company-backends '( company-clang company-semantic company-capf company-keywords company-dabbrev-code company-dabbrev))
;(setq-default company-backends '( company-yasnippet))

(setq-default company-dabbrev-time-limit 1.0)

;(defun company-yasnippet-or-completion ()
;  "Solve company yasnippet conflicts."
;  (interactive)
;  (let ((yas-fallback-behavior
;         (apply 'company-complete-common nil)))
;    (yas-expand)))
;
;(add-hook 'company-mode-hook
;          (lambda ()
;            (substitute-key-definition
;             'company-complete-common
;             'company-yasnippet-or-completion
;             company-active-map)))

;; (add-hook 'elpy-mode-hook
;; 	(lambda () (setcar company-backends '(:separate elpy-company-backend company-yasnippet)))
;; )
;(define-key company-mode-map (kbd "C-M-i") 'company-complete)
(define-key company-mode-map (kbd "C-<tab>") 'company-other-backend)

(require 'company-quickhelp)
(company-quickhelp-mode t)

(require 'company-gtags)
(setq-default company-gtags-modes (append company-gtags-modes '(julia-mode-prog-mode)))

;; (define-key company-active-map (kbd "<return>") nil)
(define-key company-mode-map (kbd "C-n") 'company-select-next)
(define-key company-mode-map (kbd "C-p") 'company-select-previous)
(define-key company-mode-map (kbd "C-<tab>") 'company-other-backend)


; Stolen from https://emacs.stackexchange.com/questions/13286/how-can-i-stop-the-enter-key-from-triggering-a-completion-in-company-mode
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Hippie-expand
;;----------------------------
(defun my-hippie-expand-completions (&optional hippie-expand-function)
      "Return the full list of possible completions generated by `hippie-expand'.
    The optional argument can be generated with `make-hippie-expand-function'."
      (let ((this-command 'my-hippie-expand-completions)
            (last-command last-command)
            (buffer-modified (buffer-modified-p))
            (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
        (flet ((ding)) ; avoid the (ding) when hippie-expand exhausts its options.
          (while (progn
                   (funcall hippie-expand-function nil)
                   (setq last-command 'my-hippie-expand-completions)
                   (not (equal he-num -1)))))
        ;; Evaluating the completions modifies the buffer, however we will finish
        ;; up in the same state that we began.
        (set-buffer-modified-p buffer-modified)
        ;; Provide the options in the order in which they are normally generated.
        (delete he-search-string (reverse he-tried-table))))
    
(defun my-ido-hippie-expand-with (hippie-expand-function)
	"Offer ido-based completion using the specified hippie-expand function."
	(let* ((options (my-hippie-expand-completions hippie-expand-function))
			(selection (and options
							(ido-completing-read "Completions: " options))))
	(if selection
		(he-substitute-string selection t)
		(message "No expansion found"))))

(defun my-ido-hippie-expand ()
	"Offer ido-based completion for the word at point."
	(interactive)
	(my-ido-hippie-expand-with 'hippie-expand))

(global-set-key (kbd "C-c /") 'my-ido-hippie-expand)

(require 'cl-lib)

(defun company-hippie-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-hippie-backend))
    (prefix (company-grab-symbol))
    (candidates (my-hippie-expand-completions))
    (meta (format "This value is named %s" arg))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Helm stuff
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
(global-set-key (kbd "C-M-x") 'execute-extended-command)
(setq-default helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x f") 'helm-find-files)
(global-set-key (kbd "C-x C-f") 'helm-multi-files)
(setq-default helm-semantic-fuzzy-match t
              helm-imenu-fuzzy-match    t)
;; (define-key evil-motion-state-map (kbd "M-p") 'helm-recentf)

(define-key helm-grep-map (kbd "C-.") 'helm-goto-next-file)
(define-key helm-grep-map (kbd "C-,") 'helm-goto-precedent-file)

;; Kind of helm related
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * ggtags
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
(define-key ggtags-mode-map (kbd "C-M-,") 'ggtags-find-tag-continue)

;(define-key evil-normal-state-map (kbd "M-.") 'ggtags-find-tag-dwim)
(define-key evil-normal-state-map (kbd "M-.") nil)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * C Stuff
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
;; * My maps
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
(define-key danny-completions (kbd "C-h") 'helm-navi-headings)

(define-key evil-visual-state-map (kbd "C-y") 'copy-and-comment-region)
(define-key evil-insert-state-map (kbd "C-y") 'copy-and-comment-line)
(define-key evil-normal-state-map (kbd "C-y") 'copy-and-comment-line)

(define-key evil-normal-state-map (kbd "M-a") 'avy-goto-char-timer)

;; (define-prefix-command 'danny-utils)
;; (define-key evil-normal-state-map (kbd "C-y") 'danny-utils)
;; (define-key danny-utils (kbd "C-y") 'copy-and-comment-region)



;; TODO I want to come back to this and have a binding that closes all *...* windows

 ;; (defun kill-dired-buffers ()
 ;; 	 (interactive)
 ;; 	 (mapc (lambda (buffer) 
 ;;           (when (eq 'dired-mode (buffer-local-value 'major-mode buffer)) 
 ;;             (kill-buffer buffer))) 
 ;;         (buffer-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Speedbar
;;----------------------------

(require 'sr-speedbar)
;(setq-default speedbar-show-unknown-files t) ; show all files
(setq-default speedbar-use-images nil) ; use text for buttons
;(setq-default sr-speedbar-right-side nil) ; put on left side
(setq-default speedbar-hide-button-brackets-flag t)
(setq-default speedbar-tag-hierarchy-method '(speedbar-sort-tag-hierarchy))

(speedbar-add-supported-extension ".jl")

;; (add-hook
;;  'speedbar-timer-hook
;;  (lambda ()
;;     (save-excursion
;;         (set-buffer speedbar-buffer)
;;         (speedbar-expand-line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * GUD
;;----------------------------

(setq-default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 ;; gdb-show-main t
 gdb-show-main nil
 )

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
;; * Org-mode stuff
;;------------------------------------------------
(require 'org)

(setq-default org-tags-column -100
              org-agenda-tags-column -100
              org-return-follows-link t
              org-log-done 'time
              org-directory "~/Dropbox/org"
              org-default-notes-file (concat org-directory "/notes.org")
			  org-habit-graph-column 70
			  org-habit-show-habits-only-for-today nil
			  org-refile-targets '((org-agenda-files :maxlevel . 3))
			  org-refile-use-outline-path t
			  org-outline-path-complete-in-steps nil
			  org-enforce-todo-dependencies t
			  org-agenda-skip-deadline-prewarning-if-scheduled t
			  org-deadline-warning-days 5
			  org-stuck-projects '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:")
			  org-agenda-skip-deadline-if-done t
			  org-agenda-skip-scheduled-if-done t)

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

;; (evil-set-initial-state 'org-agenda-mode 'normal)

(set-face-attribute 'org-level-1 nil :height 1.5 :family "Inconsolata")
(set-face-attribute 'org-level-2 nil :height 1.2 :family "Inconsolata")
;; (set-face-attribute 'outline-3 nil :height 1.2 :family "Inconsolata")

(set-face-attribute 'org-agenda-dimmed-todo-face nil :foreground "grey20")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Latex stuff
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

(when (string= (system-name) "pengix")
	(setq-default preview-orientation 'below))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Ediff stuff
;;----------------------------
(setq-default ediff-diff-options "-w")
(setq-default ediff-split-window-function 'split-window-horizontally)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * which-key mode
;;------------------------------------------------
(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-right-bottom)
(dim-minor-name 'which-key-mode nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Outshine/outorg/navi-mode stuff
;;----------------------------

(require 'outshine)
;; (add-hook 'outline-minor-mode-hook
;; 		  (lambda () (progn
;; 					(outshine-hook-function)
;; 					(setq-local outshine-imenu-preliminary-generic-expression
;; 						`((nil ,(concat (message "%s" (outshine-calc-outline-regexp)) "\\(.*$\\)") 1))))))
(add-hook 'outline-minor-mode-hook
		  (lambda () (progn
					;; (outshine-hook-function)
					(outshine-mode)
					(setq-local outshine-imenu-preliminary-generic-expression
						`((nil ,(concat (outshine-calc-outline-regexp) "\\(.*$\\)") 1))))))
;; (add-hook 'outline-minor-mode-hook
;; 		  (lambda () (setq-local outshine-imenu-preliminary-generic-expression
;; 						`((nil ,(concat (outshine-calc-outline-regexp) "\\(.*$\\)") 1)))))
;; (add-hook 'outline-minor-mode-hook (lambda ()
;; 									 (unless (eq major-mode 'org-mode) (progn (outshine-hook-function) (message "%s" "looks like this was not org-mode!")))))
;(setq outshine-imenu-preliminary-generic-expression
;	`((nil ,(concat (outshine-calc-outline-regexp) "\\(.*$\\)") 1)))

(add-hook 'prog-mode-hook 'outline-minor-mode)
(dim-minor-name 'outline-minor-mode nil)

(set-face-attribute 'outline-1 nil :height 2.0 :family "Inconsolata" :foreground "black" :background "DarkSeaGreen1")
(set-face-attribute 'outline-2 nil :height 1.5 :family "Inconsolata")
(set-face-attribute 'outline-3 nil :height 1.2 :family "Inconsolata")

(setq outshine-imenu-show-headlines-p nil)
; Need this to help with outshine-imenu
;; (add-hook 'outline-minor-mode-hook (lambda () (setq-local outshine-imenu-preliminary-generic-expression
;;                `((nil ,(concat (message "%s" (outshine-calc-outline-regexp)) "\\(.*$\\)") 1)))))

;; Fix self-insert-command for evil-mc
(add-to-list 'evil-mc-custom-known-commands '(outshine-self-insert-command . ((:default . evil-mc-execute-default-call-with-count))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Julia
;;----------------------------
(require 'julia-mode)
(add-hook 'julia-mode-hook 'ggtags-mode)
;; (add-hook 'julia-mode-hook (lambda () (setq-local ggtags-process-environment '("GTAGSLABEL=juliactags"))))
; TODO: I should fix this up for that it uses something like the default julia-mode settings but handles my macro prefixes.
;; (add-hook 'julia-mode-hook (lambda () (setq-local imenu-create-index-function #'ggtags-build-imenu-index)))


; Fixing up imenu in julia-mode to work with my macros
(let ((macroprefix "^\\(?:[[:blank:]]*@[^[:blank:]@]+\\)*"))
	(setq julia-imenu-generic-expression
	;; don't use syntax classes, screws egrep
	`(
		;("Function (_)" ,(concat macroprefix "[[:blank:]]*function[[:blank:]]+\\(_[^[:blank:]]*\\)") 1)
		("Function" ,(concat macroprefix "[[:blank:]]*function[[:blank:]]+\\(.*)\\)[[:blank:]]*") 1)
		("Function" ,(concat macroprefix "[[:blank:]]*\\([^[:blank:](]*(.*)\\)[[:blank:]]*=[^=]*") 1)
		("Const" "[ \t]*const \\([^ \t\n]*\\)" 1)
		;; ("Type"  ,(concat macroprefix "^[ \t]*[a-zA-Z0-9_]*type[a-zA-Z0-9_]* \\([^ \t\n]*\\)") 1)
		("Struct" ,(concat macroprefix "\\(?:[[:blank:]]*mutable\\)?[[:blank:]]+struct[[:blank:]]+\\([^{[:blank:]\n]+\\)") 1)
		;; ("Require"      " *\\(\\brequire\\)(\\([^ \t\n)]*\\)" 2)
		;; ("Include"      ,(concat macroprefix " *\\(\\binclude\\)(\\([^ \t\n)]*\\)") 2)
		("Using"      ,(concat macroprefix "[[:blank:]]*using[[:blank:]]*\\(.*\\)") 1)
		)))

(add-hook 'julia-mode-hook (lambda () (setq-local company-backends '( company-gtags company-dabbrev-code))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Key bindings
;;----------------------------


(define-prefix-command 'danny-orgmode)
(define-key danny-orgmode (kbd "<f7>") 'danny-open-orgfile)
(define-key danny-orgmode "l" 'org-store-link)
(define-key danny-orgmode "a" 'org-agenda)
(define-key danny-orgmode "c" 'org-capture)
(define-key danny-orgmode "b" 'org-iswitchb)
(define-key danny-orgmode "j" 'org-clock-goto)
(define-key danny-orgmode "o" 'org-clock-out)
(define-key danny-orgmode "i" 'org-clock-in-last)
(define-key danny-orgmode "r" 'remember)
(define-key danny-orgmode "R" 'remember-notes)


(define-prefix-command 'danny-projectile)
(define-key danny-projectile (kbd "<f9>") 'helm-projectile-switch-project)
(define-key danny-projectile "f" 'helm-projectile-find-file-dwim)
(define-key danny-projectile "p" 'org-publish-current-project)
(define-key danny-projectile "a" 'helm-projectile-ag)

(global-set-key (kbd "<f7>") 'danny-orgmode)
(global-set-key (kbd "<f9>") 'danny-projectile)
(global-set-key (kbd "<f6>") 'magit-status)
(global-set-key (kbd "<f12>") 'switch-to-minibuffer-window)
(global-set-key (kbd "<f8>") 'sr-speedbar-toggle)


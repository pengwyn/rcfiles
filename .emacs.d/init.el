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
    material-theme
    elpy
    ein
    evil
    evil-surround
    evil-numbers
    evil-visualstar
    evil-exchange
    evil-search-highlight-persist
    magit
    relative-line-numbers))


(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

(elpy-enable)
(elpy-use-ipython)
(setq elpy-rpc-python-command "python2")
(elpy-use-ipython "ipython2")

(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
;(global-linum-mode t) ;; enable line numbers globally
(global-relative-line-numbers-mode)

;; init.el ends here

;; Danny stuff
;(autoload 'python-mode "python-mode.el" "Python mode." t)
;(setq auto-mode-alist (append '(("/.*\.py\'" . python-mode)) auto-mode-alist))

(setq python-shell-interpreter "ipython2"
    python-shell-interpreter-args "--simple-prompt -i")

(require 'ein)

(require 'evil)
(evil-mode 1)

(require 'evil-surround)
(global-evil-surround-mode 1)

(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-visual-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)

(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
(define-key evil-visual-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

(require 'evil-exchange)
(setq evil-exchange-key (kbd "zx"))
(evil-exchange-install)

(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)
(setq evil-search-highlight-string-min-len 3)


 (defun evil-toggle-input-method ()
      "when toggle on input method, switch to evil-insert-state if possible.
    when toggle off input method, switch to evil-normal-state if current state is evil-insert-state"
      (interactive)
      (if (not evil-mode) (turn-on-evil-mode) (turn-off-evil-mode)))
    
(global-set-key (kbd "C-\\") 'evil-toggle-input-method)

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

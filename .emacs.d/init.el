;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    material-theme
    elpy))

(elpy-enable)
(elpy-use-ipython)
(setq elpy-rpc-python-command "python2")
(elpy-use-ipython "ipython2")

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally

;; init.el ends here

;; Danny stuff
;(autoload 'python-mode "python-mode.el" "Python mode." t)
;(setq auto-mode-alist (append '(("/.*\.py\'" . python-mode)) auto-mode-alist))

(setq python-shell-interpreter "ipython"
    python-shell-interpreter-args "--simple-prompt -i")

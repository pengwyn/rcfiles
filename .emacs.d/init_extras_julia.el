
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Julia
;;----------------------------
(when (mypackages-p)
  (setq-default julia-mode-use-poly t)
  (add-to-list 'load-path (concat mypackages-path "julia-emacs/"))
  (use-package polymode)
  (require 'julia-mode))

(use-package julia-mode
  :init
  (define-prefix-command 'my/julia-ext-map nil "Julia ext int map")

  :hook (
         ;; (julia-mode . ggtags-mode)
         (julia-mode . julia-math-mode)
         (julia-mode . julia-repl-mode)
         (julia-repl . julia-repl-use-emacsclient)
         (julia-mode . highlight-symbol-mode))
  
  :custom
  (julia-max-block-lookback 50000)

  :bind (:map julia-mode-map
         ("<f5>" . my/julia-ext-map))

  :config
  (bind-key "<f5>" 'julia-repl 'my/julia-ext-map)
  (bind-key "b" 'my/julia-set-bp 'my/julia-ext-map)

  (let ((sysimg-file "/home/pengwyn/.julia/config/ijulia_sysimage.so"))
    (when (file-exists-p sysimg-file)
      (setq julia-repl-switches "-i --color=yes --sysimage=/home/pengwyn/.julia/config/ijulia_sysimage.so")))

  (defun my/julia-set-bp ()
    (interactive)
    "Send a breakpoint set command to the julia-repl"
    (let ((filename (buffer-file-name))
          (lineno (int-to-string (line-number-at-pos nil t))))
      (julia-repl--send-string (concat "breakpoint(\"" filename "\"," lineno ")")))
    )

  (use-package julia-repl)

  (define-key LaTeX-math-keymap "'" 'LaTeX-math-prime)
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * My packages
;;----------------------------

(use-package julia-profile
  :load-path (lambda () (concat mypackages-path "julia-profile/"))
  :bind (:map my/julia-ext-map
         ("p" . #'julia-profile-add-all-deferred)))

(use-package julia-funcobs
  :load-path "~/work5/julia_packages/FunctionObserving/src"
  :bind (:map my/julia-ext-map
         ("o" . #'julia-function-observe)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * eglot
;;----------------------------
(use-package eglot-jl
  :config
  (eglot-jl-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * LSP
;;----------------------------

(add-to-list 'load-path "~/.emacs.d/mypackages/lsp-julia/")

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "s-i")
  (setq lsp-julia-package-dir nil)

  :hook ((lsp-mode . lsp-enable-which-key-integration)
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

  (use-package lsp-julia)
  )

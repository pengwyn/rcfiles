(use-package posframe
  :config
  (defun my/posframe-arghandler (_ arg-name value)
    (cl-case arg-name
      (:internal-border-width 2)
      (:internal-border-color "red")
      ;; (:font (font-spec :size 10))
      (:poshandler #'posframe-poshandler-point-bottom-left-corner)
      ;; (:poshandler #'posframe-poshandler-frame-top-right-corner)
      (:min-height (max (or value 0) 3))
      ;; (:width (or value 999))
      (:min-width (- (window-width) 2))
      (otherwise value)))
  (setq posframe-arghandler #'my/posframe-arghandler)
  )

(use-package which-key-posframe
  :after which-key posframe
  :config
  (which-key-posframe-mode))

;; TODO: Need to make this pop up a new minibuffer in the posframe to allow for recursive editing
(use-package helm-posframe
  ;; :after helm posframe
  :config
  (helm-posframe-enable))

(use-package hydra-posframe
  ;; :after hydra posframe
  :load-path "~/.emacs.d.rem/mypackages/hydra-posframe"
  :config
  (hydra-posframe-mode))

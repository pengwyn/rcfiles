;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Org-mode stuff
;;------------------------------------------------
(use-package org
  :init (require 'org-agenda)
         (require 'org-habit)
         (define-prefix-command 'danny-orgmode nil "Org-mode useful things")
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
  (unless (boundp 'recentf-exclude) (setq recentf-exclude nil))
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

  (evil-define-key 'insert org-mode-map (kbd "<shift> <tab>") 'org-indent-item)
  (evil-define-key 'insert org-mode-map (kbd "<S-tab>") 'org-indent-item)
  (evil-define-key 'insert org-mode-map (kbd "<S-iso-lefttab>") 'org-indent-item)
  (evil-define-key 'insert org-mode-map (kbd "<backtab>") 'org-indent-item)


  (setq org-modules (append org-modules '(org-habit org-mouse)))

  (when (boundp 'danny-add-prettify-greek)
    (danny-add-prettify-greek 'org-mode-hook))
  (add-hook 'org-mode-hook (lambda () "Beautify Org Checkbox Symbol"
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

  (use-package poly-org
    :config
    (set-slot-value poly-org-innermode 'adjust-face 10)
    )
  
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

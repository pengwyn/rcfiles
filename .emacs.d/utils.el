;; Stolen from somewhere.
(defmacro define-key-with-fallback (keymap key def condition &optional mode)
  "Define key with fallback. Binds KEY to definition DEF in keymap KEYMAP, 
   the binding is active when the CONDITION is true. Otherwise turns MODE off 
   and re-enables previous definition for KEY. If MODE is nil, tries to recover 
   it by stripping off \"-map\" from KEYMAP name."
  `(define-key ,keymap ,key
     (lambda () (interactive)
       (if ,condition ,def
         (let* ((,(if mode mode
                    (let* ((keymap-str (symbol-name keymap))
                           (mode-name-end (- (string-width keymap-str) 4)))
                      (if (string= "-map" (substring keymap-str mode-name-end))
                          (intern (substring keymap-str 0 mode-name-end))
                        (error "Could not deduce mode name from keymap name (\"-map\" missing?)")))) 
                 nil)
                (original-func (key-binding ,key)))
           (message "About to call %s" original-func)
           (call-interactively original-func))))))


(defun my/evil-add-bindings (map)
  "Adds the usual evil hjkl and some other things I like"
  (evil-add-hjkl-bindings map 'emacs
    (kbd "/") 'evil-search-forward
    (kbd "n") 'evil-search-next
    (kbd "N") 'evil-search-previous
    (kbd "^") 'evil-first-non-blank
    (kbd "^") 'evil-end-of-line
    (kbd "C-f") 'evil-scroll-page-down
    (kbd "C-b") 'evil-scroll-page-up))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Align func
;;----------------------------
(defun my/simple-align (beg end)
     "Align using any 2+ space with 4 spaces"
     (interactive "r")
     (align-regexp beg end (rx (group (or
                                       (>= 2 (syntax whitespace))
                                       (group (* (syntax whitespace)) (1+ "\t") (* (syntax whitespace))))
                                      )) 1 4 t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Get column at arbitrary point
;;----------------------------

(defun col-at-point (point)
  (save-excursion (goto-char point) (current-column)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Quick anonymous functions
;;----------------------------

;; (require 'dash)

;; (defun $-find-args (seq)
;;   (seq-sort
;;    (lambda (sym1 sym2)
;;      (< (string-to-number (substring (symbol-name sym1) 1))
;;         (string-to-number (substring (symbol-name sym2) 1))))
;;    (seq-filter
;;     (lambda (x)
;;       (and (symbolp x) (equal 0 (string-match "\\$[0-9]+" (symbol-name x)))))
;;     (-flatten seq))))

;; (defmacro $ (&rest body)
;;   "Shortcut for lambdas.

;; Inside this form symbols in the form $N where N is a positive
;; integer are to stand for positional arguments to the generated
;; lambda.

;; If the car of the BODY is a vector though, that vector becomes
;; the argument list of the new lambda."
;;   (let ((head (car body))
;;         (tail (cdr body))
;;         args the-body)
;;     (if (vectorp head)
;;         ;; Convert it to a list.
;;         (setf args (seq-into head 'list)
;;               the-body tail)
;;       (setf args ($-find-args body)
;;             the-body body))
;;     `(lambda ,args ,@the-body)))

;; (defmacro $i (&rest body)
;;   "Danny did this for quick interactive macro."
;;   (let ((new-body (cons '(interactive) body)))
;;     `($ ,@new-body)))

;; TODO: The above doesn't work for use-package... annoying. Need to fix it

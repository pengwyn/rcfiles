
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Prettify
;;----------------------------
(use-package prettify-greek
  :custom
  (prettify-symbols-unprettify-at-point "right-edge"))

                                        ; Make up my own set with \ in front of them.
(defconst my/prettify-set 
  (let* ((my-greek (append (copy-tree prettify-greek-lower) (copy-tree prettify-greek-upper))))
    (dolist (item my-greek)
      (setcar item (concat "\\" (car item))))
    (append my-greek prettify-greek-lower prettify-greek-upper)))

(defun my/prettify-predicate (start end _match) "Only care about words and not symbols."
       ;; (not (or (= (char-syntax (char-after end)) ?w)
       ;;           (= (char-syntax (char-before start)) ?w))))
       (not (or (string-match-p "[a-zA-Z]" (string (char-after end)))
                (string-match-p "[a-zA-Z]" (string (char-before start))))))


;; mu test
(defun my/add-prettify-greek () "Add prettify-greek symbols to mode."
       (setq ;prettify-symbols-alist (append prettify-symbols-alist prettify-greek-lower prettify-greek-upper)))))
        prettify-symbols-alist (append prettify-symbols-alist my/prettify-set))
       (prettify-symbols-mode t)
       (setq prettify-symbols-compose-predicate 'my/prettify-predicate))

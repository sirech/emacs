;;; defuns-search --- Functions related to search

;; Credit to http://sachachua.com/wp/2008/07/emacs-keyboard-shortcuts-for-navigating-code/
(defun isearch-yank-current-word ()
  "Pull current word from buffer into search string."
  (interactive)
  (save-excursion
    (skip-syntax-backward "w_")
    (isearch-yank-internal
     (lambda ()
       (skip-syntax-forward "w_")
       (point)))))


(provide 'defuns-search)

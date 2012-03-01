;;; lang-coffee.el --- Configuration for Coffee Script

;; Set hooks

(add-hook 'coffee-mode-hook 'run-coding-hook)
(add-hook 'coffee-mode-hook 'custom-coffee)

;; Activation

;; Initialization

(defun custom-coffee ()
  (set (make-local-variable 'tab-width) 2))

(provide 'lang-coffee)
;;; lang-coffee.el ends herea


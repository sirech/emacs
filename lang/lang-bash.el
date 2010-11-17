;;; lang-bash.el --- Configuration for Bash

;; Set hooks

(add-hook 'sh-mode-hook 'run-coding-hook)
(add-hook 'sh-mode-hook 'bash-activate-ide)

;; Activation

(defun bash-activate-ide ()
  (turn-on-autocomplete)
  (turn-on-flymake nil))

;; Initialization
(defvar flymake-bash-err-line-patterns
      '(("^\\(.+\\): line \\([0-9]+\\): \\(.+\\)$" 1 2 nil 3)))

(eval-after-load 'flymake
  '(setup-flymake nil "sh" 'flymake-bash-init nil flymake-bash-err-line-patterns))

(defun flymake-bash-init ()
  (custom-flymake-init "bash" "-n"))

(provide 'lang-bash)
;;; lang-bash.el ends herea

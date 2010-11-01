;;; shell-utils.el --- Convenience functions for the shell mode

;; (make-face 'shell-base-face)
;; (set-face-background 'shell-base-face "black")
;; (set-face-foreground 'shell-base-face "white")

;; (change-face-for-mode 'shell-mode-hook 'shell-base-face)

;; Modes

(defun change-face-for-mode (mode-hook face)
  "Changes the face used for a mode, using an existing face and the mode hook"
  (when (> emacs-major-version 22)
  (add-hook mode-hook
            '(lambda ()
               (buffer-face-mode t)
               (buffer-face-set 'face)))))

(provide 'shell-utils)
;;; shell-utils.el ends here

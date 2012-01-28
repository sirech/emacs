;;; appearance.el --- How emacs looks

(setq visible-bell t
      font-lock-maximum-decoration t
      inhibit-startup-message t
      color-theme-is-global t
      truncate-partial-width-windows nil)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1)
  (column-number-mode 't))

(add-hook 'before-make-frame-hook 'turn-off-tool-bar)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Support visual lines
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Cosmetics

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'mumamo
  '(eval-after-load 'zenburn
     '(ignore-errors (set-face-background
                      'mumamo-background-chunk-submode "gray22"))))

(provide 'appearance)

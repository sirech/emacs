;;; theme.el --- A theme to be used with color-theme

(defun color-theme-sirech ()
  "This color theme is inspired by the old jedit-grey theme,
which is a bit too dark for my liking"
  (interactive)
  (color-theme-install
   '(color-theme-jedit-grey
     ((font . "fixed")
      (width . 130)
      (height . 50)
      (background-color . "grey86")
      (foreground-color . "black")
      (background-mode . light)
      (mouse-color . "black")
      (cursor-color . "yellow1")
      )
     (default ((t (nil))))
     (font-lock-comment-face ((t (:italic t :foreground "RoyalBlue4"))))
     (font-lock-string-face ((t (:foreground "Gold4"))))
     (font-lock-keyword-face ((t (:bold t :foreground "DarkRed"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
     (font-lock-constant-face ((t (:foreground "DarkCyan"))))
     (font-lock-type-face ((t (:foreground "DarkRed"))))
     (font-lock-variable-name-face ((t (:foreground "Blue"))))
     (font-lock-function-name-face ((t (:foreground "Green4"))))
     (font-lock-builtin-face ((t (:bold t :foreground "DarkRed"))))
     (highline-face ((t (:background "grey84"))))
     (setnu-line-number-face ((t (:background "White" :foreground "MediumPurple3" :italic t))))
     (show-paren-match-face ((t (:background "grey60"))))
     (region ((t (:background "grey70"))))
     (highlight ((t (:background "PaleGreen" :foreground "DarkGreen"))))
     (secondary-selection ((t (:background "white"))))
     (widget-field-face ((t (:background "royalblue"))))
     (widget-single-line-field-face ((t (:background "royalblue")))))) )

(provide 'theme)
;;; theme.el ends here

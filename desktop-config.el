;;; desktop-config.el --- Save and Restore the session when emacs is
;;; restarted

;; save a list of open files in ~/.emacs.d/.emacs.desktop
(setq desktop-path (list dotfiles-dir))
(desktop-save-mode 1)
(defadvice desktop-read (around trace-desktop-errors)
  (let ((old-debug-on-error debug-on-error))
    (setq debug-on-error t)
    ad-do-it
    (setq debug-on-error old-debug-on-error)))

(provide 'desktop-config)
;;; desktop-config.el ends here

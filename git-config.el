;;; git-config.el --- Configuration for git

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)
(autoload 'magit-status "magit")

(provide 'git-config)
;;; git-config.el ends here

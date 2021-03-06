;;; linux.el --- Specific settings for Linux

;; fix for Ubuntu 10.10 problem with flyspell
(setq flyspell-issue-welcome-flag nil)

;; ACK is called differently for linux
(eval-after-load 'full-ack
  '(progn
    (setq ack-executable (executable-find "ack-grep"))))

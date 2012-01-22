;;; eshell-utils.el --- Making the defaults a bit saner
;;

(setq eshell-cmpl-cycle-completions nil
      eshell-save-history-on-exit t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

(setq eshell-aliases-file (concat dotfiles-dir "eshell-alias"))

(make-face 'eshell-base-face)
(set-face-background 'eshell-base-face "black")
(set-face-foreground 'eshell-base-face "white")

(eval-after-load 'esh-opt
  '(progn
     (require 'em-prompt)
     (require 'em-term)
     (require 'em-cmpl)
     (setenv "PAGER" "cat")
     (set-face-attribute 'eshell-prompt nil :foreground "turquoise1")
     ;; Change face for eshell -- emacs 23 only
     (when (> emacs-major-version 22)
       (add-hook 'eshell-mode-hook
                 '(lambda ()
                    (buffer-face-mode t)
                    (buffer-face-set 'eshell-base-face))))
     (when (< emacs-major-version 23)
       (add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
                 '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))
       (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color))
     ;; TODO: submit these via M-x report-emacs-bug
     (add-to-list 'eshell-visual-commands "ssh")
     (add-to-list 'eshell-visual-commands "tail")
     (add-to-list 'eshell-command-completions-alist
                  '("gunzip" "gz\\'"))
     (add-to-list 'eshell-command-completions-alist
                  '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))))

(defun eshell/cds ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory "src")))

;; Credit to http://www.khngai.com/emacs/eshell.php
(defun eshell/clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; Port features from
;; http://blog.peepcode.com/tutorials/2009/shell-method-missing/shell_method_missing.rb
;; * cloning git repos, github repos
;; * downloading http urls
;; * extracting archives
;; * changing to directories

(provide 'eshell-utils)
;;; eshell-utils.el ends here

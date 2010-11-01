;;; ac-config.el --- Configuration for the auto-complete module
;;

;; HELPER FUNCTIONS

(defun auto-complete-dir ()
  (let ((location (locate-library "auto-complete")))
    (substring location
               0
               (- (length location) (length "auto-complete.el")))))

(defun turn-on-autocomplete ()
  (interactive)
  (require 'auto-complete)
  (auto-complete-mode 1))

;; SETTINGS

;; I don't know how to avoid repeating the functions, as the
;; ac-*-mode-setup functions don't exist until auto-complete-config is
;; imported, and I'm trying to avoid that
(defun ac-lisp-setup-hook ()
  (require 'auto-complete-config)
  (turn-on-autocomplete)
  (ac-emacs-lisp-mode-setup))

(defun ac-cc-setup-hook ()
  (require 'auto-complete-config)
  (turn-on-autocomplete)
  (ac-cc-mode-setup))

(defun ac-css-setup-hook ()
  (require 'auto-complete-config)
  (turn-on-autocomplete)
  (ac-css-mode-setup))

(add-hook 'emacs-lisp-mode-hook 'ac-lisp-setup-hook)
(add-hook 'c-mode-common-hook 'ac-cc-setup-hook)
(add-hook 'css-mode-hook 'ac-css-setup-hook)
(add-hook 'sh-mode-hook 'turn-on-autocomplete)

(eval-after-load 'auto-complete
  '(progn
     (add-to-list 'ac-dictionary-directories (concat (auto-complete-dir) "ac-dict"))
     (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
     (setq ac-comphist-file (expand-file-name "~/.ac-comphist"))
     (set-face-background 'ac-candidate-face "white")
     (setq ac-override-local-map t)
     (setq ac-use-menu-map t)
     (setq ac-ignore-case t)
     (setq ac-menu-height 10)
     (setq ac-dwim nil)))

;; KEYBINDINGS

(eval-after-load 'auto-complete
  '(progn
     (global-set-key (kbd "M-s") 'auto-complete)
     (define-key ac-complete-mode-map "\C-g" 'ac-stop)
     (define-key ac-complete-mode-map "\r" 'ac-complete)
     (define-key ac-menu-map "\C-n" 'ac-next)
     (define-key ac-menu-map "\C-p" 'ac-previous)))

(provide 'ac-config)
;;; ac-config.el ends here


;;; ac-config.el --- Configuration for the auto-complete module
;;
;; This settings have some external dependencies:
;;  * yasnippet:
;;   - Download normal install from
;; http://code.google.com/p/yasnippet/
;;   - extract to a directory in the load-path (preferably vendor/),
;; and rename folder to yasnippet

;; Check availability
(defun yasnippet-is-present ()
  "Determines if yasnippet is installed"
  (locate-library "yasnippet"))

;; HELPER FUNCTIONS

(defun activate-yasnippet ()
  (when (yasnippet-is-present)
    (require 'yasnippet)
    (when ac-sources
      (setq-default ac-sources (append '(ac-source-yasnippet) ac-sources)))))

;; SETTINGS

;; YASnippet is included here
(eval-after-load 'yasnippet
  '(progn
     (yas/initialize)
     (yas/load-directory (concat (locate-library-parent-dir "yasnippet") "snippets"))
     ;; (yas/load-directory yas/root-directory)
     (yas/minor-mode-on)
     ;; (add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand)
     ;; (setq yas/prompt-functions '(yas/x-prompt yas/dropdown-prompt))
     ))

(eval-after-load 'auto-complete
  '(progn
     (add-to-list 'ac-dictionary-directories (concat (locate-library-parent-dir "auto-complete") "ac-dict"))
     (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
     (activate-yasnippet)
     (require 'pos-tip)
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


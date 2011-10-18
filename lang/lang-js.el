;;; lang-js.el --- Configuration for JavaScript
;;
;; This settings have some external dependencies:
;;  * spidermonkey: Needs to be in the path.
;;
;;  * jslint (Modified version):
;;   - http://whereisandy.com/code/jslint/
;;   - put the file in the lib directory, renaming it to jslint.js
;;   - you might need to edit the first line to something like:
;;     #!/usr/bin/env js (fix path issues)
;;
;; The functionality is turned on only if the required dependencies
;; can be found.

;; Attach mode

;; Using espresso
;; (add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
;; (add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

;; If you prefer js2-mode, use this instead:
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;; Set hooks

(add-hook 'espresso-mode-hook 'run-coding-hook)
(add-hook 'espresso-mode-hook 'moz-minor-mode)
(add-hook 'espresso-mode-hook 'esk-paredit-nonlisp)

(add-hook 'js2-mode-hook 'run-coding-hook)
(add-hook 'js2-mode-hook 'moz-minor-mode)

;; Check availability

(defvar jslint-location (concat lib-dir "jslint.js")
  "The location of the jslint library")

(defun jslint-is-present ()
  "Determines if jslint is present in the system"
  (file-exists-p jslint-location))

;; Initialization

;; (eval-after-load 'espresso
;;   '(progn
;;      (define-key espresso-mode-map "{" 'paredit-open-curly)
;;      (define-key espresso-mode-map "}" 'paredit-close-curly-and-newline)
;;      ;; fixes problem with pretty function font-lock
;;      (define-key espresso-mode-map (kbd ",") 'self-insert-command)
;;      (font-lock-add-keywords
;;       'espresso-mode `(("\\(function *\\)("
;;                         (0 (progn (compose-region (match-beginning 1)
;;                                                   (match-end 1) "ƒ")
;;                                   nil)))))))

;; Check code using JSLint, credit to http://codesnippets.joyent.com/posts/show/8031

;;; Using the modified jslint from http://whereisandy.com/code/jslint/
;;; and spidermonkey, easy linting of js files,
;;;
;;; M-j calls jslint and outputs to a split window. Within that
;;; window M-j will find the next lint issue and jump to that
;;; line in the file

;; TODO: use *compilation*, like http://stackoverflow.com/questions/2465705/emacs-can-i-set-compilation-error-regexp-alist-in-a-mode-hook-fn
(eval-after-load 'js2-mode
  '(progn
     (setq js2-highlight-level 3)
     (setq js2-basic-offset 2)
     (setq js2-cleanup-whitespace t)
     (setq js2-indent-on-enter-key t)

     (when (jslint-is-present)

       (setq *jslint-target-buffer* nil)

       (defun jslint ()
         (interactive)
         (let ((jsbuffer-name "*jslint-minibuffer*"))
           (if (string= (buffer-name) jsbuffer-name)
               (progn
                 (if (re-search-forward "Lint at line \\([0-9]+\\)")
                     (let ((num (match-string 1)))
                       (goto-line (string-to-number num) *jslint-target-buffer*))))
             (let* ((filename (buffer-file-name))
                    (suffix (downcase (file-name-extension filename))))
               (setq *jslint-target-buffer* (current-buffer))
               (if (and filename (string= suffix "js"))
                   (let ((result (shell-command-to-string (concat jslint-location " < " filename))))
                     (if (string= result "jslint: No problems found.\n")
                         (progn
                           (message "jslint: No problems found.")
                           (if (get-buffer jsbuffer-name)
                               (kill-buffer (get-buffer jsbuffer-name))))
                       (let ((buff (get-buffer-create jsbuffer-name)))
                         (switch-to-buffer-other-window buff)
                         (erase-buffer)
                         (princ result buff)
                         (goto-char 0)
                         ))))
               (message "Warning: Not a file buffer or a .js file.")))))

       ;;
       ;; KEYBINDINGS
       ;;

       ;; VS-style compile controls
       (define-key js2-mode-map (kbd "<f7>") 'jslint))))

(provide 'lang-js)
;;; starter-kit-js.el ends here

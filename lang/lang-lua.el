;;; lang-lua.el --- Configuration for Lua
;;
;; The syntax highlight requires the following:
;;  * luac
;;   - install luac
;;   - include the binary in the path used by emacs with (add-to-list 'exec-path "path")
;;
;; The functionality is turned on only if the required
;; dependencies can be found.

;; Set hooks

(add-hook 'lua-mode-hook 'run-coding-hook)
(add-hook 'lua-mode-hook 'lua-activate-ide)

;; Check availability

(defun luac-is-present ()
  "Determines if the luac interpreter is present in the system"
  (executable-find "luac"))

;; Activation

(defun lua-activate-ide ()
  (turn-on-flymake 'luac-is-present))

;; Initialization

(defvar flymake-lua-err-line-patterns '(("^.*luac[0-9.]*\\(.exe\\)?: *\\(.*\\):\\([0-9]+\\): \\(.*\\)$"
                                         2 3 nil 4)))

(eval-after-load 'flymake
  '(setup-flymake 'luac-is-present "lua" 'flymake-lua-init nil flymake-lua-err-line-patterns))

;; Auto Syntax Error highlight, loosely based on http://github.com/sroccaserra/emacs/blob/master/flymake-lua.el

(defun flymake-create-temp-in-system-tempdir (filename prefix)
  (make-temp-file (or prefix "flymake-lua")))

(defun flymake-lua-init ()
  (list "luac"
        (list "-p" (flymake-init-create-temp-buffer-copy
                    'flymake-create-temp-in-system-tempdir))))

;; For some reason this init function does not work

;; (defun flymake-lua-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                      'flymake-create-temp-inplace))
;;          (local-file (file-relative-name
;;                       temp-file
;;                       (file-name-directory buffer-file-name))))
;;     (list luac (list "-p" local-file))))

(provide 'lang-lua)
;;; lang-lua.el ends here

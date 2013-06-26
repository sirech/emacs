;;; lang-lua.el --- Configuration for Lua
;;
;; Flycheck:
;;  * install luac
;;
;; The functionality is turned on only if the required
;; dependencies can be found.

;; Set hooks

(add-hook 'lua-mode-hook 'run-coding-hook)

(provide 'lang-lua)
;;; lang-lua.el ends here

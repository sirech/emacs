;;; lang-js.el --- Configuration for JavaScript
;;
;; Flycheck:
;;  * jshint:
;;   - install nodejs
;;   - npm install -g jshint
;;
;; The functionality is turned on only if the required dependencies
;; can be found.

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;; Initialization

(setq js2-basic-offset 2)
(setq js2-bounce-indent t)
(setq js2-cleanup-whitespace t)
(setq js2-highlight-level 3)
(setq js2-indent-on-enter-key t)

(provide 'lang-js)
;;; starter-kit-js.el ends here

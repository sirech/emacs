;;; el-get-config.el --- Configuration for the el-get package manager
;;
;; The package is downloaded automatically from the internet if it is
;; not already present.
;;
;; If the process is not working, simply evaluate the url-retrieve
;; statement in a buffer.

(add-to-loadpath (concat dotfiles-dir "el-get") t)

(unless (require 'el-get nil t)
 (url-retrieve
  "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
  (lambda (s)
   (let (el-get-master-branch)
    (end-of-buffer)
    (eval-print-last-sexp))))
 )

(el-get 'sync)

(provide 'el-get-config)
;;; el-get-config.el ends here

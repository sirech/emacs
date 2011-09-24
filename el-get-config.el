;;; el-get-config.el --- Configuration for the el-get package manager
;;
;; Requires git clone git@github.com:sirech/el-get.git

(add-to-loadpath (concat dotfiles-dir "el-get") t)

(require 'el-get)
(setq el-get-dir (concat dotfiles-dir "el-get-packages"))


;; Packages
(setq packages-modes
      '(
        crontab-mode
        css-mode
        csv-mode
        js2-mode
        lua-mode
        markdown-mode
        paredit
        yaml-mode))

(setq packages-ruby
      '(
        inf-ruby
        rinari
        ruby-compilation
        ruby-electric
        ruby-mode
        rvm
        ))

(setq packages-utilities
     '(
       full-ack
       smex
       ))

(setq packages-other
      '(
        auto-complete
        gist
        htmlize
        magit
        psvn
        ))

(el-get 'sync packages-modes packages-ruby packages-utilities packages-other)

(provide 'el-get-config)
;;; el-get-config.el ends here

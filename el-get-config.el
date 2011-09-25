;;; el-get-config.el --- Configuration for the el-get package manager
;;
;; Requires git clone git@github.com:sirech/el-get.git

(add-to-loadpath (concat dotfiles-dir "el-get") t)

(require 'el-get)
(setq el-get-dir (concat dotfiles-dir "el-get-packages"))


;; PACKAGES LIST

(setq packages-modes
      '(
        crontab-mode
        css-mode
        csv-mode
        js2-mode
        lua-mode
        markdown-mode
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

;; With initialization
(setq el-get-sources
      '(
        (:name paredit
               :after (lambda ()
                        (define-key paredit-mode-map (kbd "M-s") 'nil)))
        ))

(el-get 'sync
        packages-modes
        packages-ruby
        packages-utilities
        packages-other
        (loop for src in el-get-sources collect (el-get-source-name src)))

(provide 'el-get-config)
;;; el-get-config.el ends here

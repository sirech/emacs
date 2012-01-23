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
        git-commit-mode
        haskell-mode
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
        rspec-mode
        rvm
        yari
        ))

(setq packages-utilities
      '(
        full-ack
        smex
        ))

(setq packages-other
      '(
        auto-complete
        auto-complete-ruby
        color-theme
        fringe-helper
        gist
        htmlize
        ibuffer-vc
        magit
        psvn
        pos-tip
        ))

;; With initialization
(setq el-get-sources
      '(
        (:name auto-complete
               :after (lambda ()
                        (global-set-key (kbd "M-s") 'auto-complete)
                        (define-key ac-complete-mode-map "\C-g" 'ac-stop)
                        (define-key ac-complete-mode-map "\r" 'ac-complete)
                        (define-key ac-menu-map "\C-n" 'ac-next)
                        (define-key ac-menu-map "\C-p" 'ac-previous)

                        (setq ac-comphist-file (expand-file-name "~/.ac-comphist"))
                        (set-face-background 'ac-candidate-face "white")
                        (setq ac-override-local-map t)
                        (setq ac-use-menu-map t)
                        (setq ac-ignore-case t)
                        (setq ac-menu-height 10)
                        (setq ac-dwim nil)))

        (:name paredit
               :after (lambda ()
                        (define-key paredit-mode-map (kbd "M-s") 'nil)))

        (:name yari
               :after (lambda ()
			(eval-after-load 'ruby-mode
			  '(define-key ruby-mode-map [f1] 'yari))))
        ))

(el-get 'sync
        packages-modes
        packages-ruby
        packages-utilities
        packages-other
        (loop for src in el-get-sources collect (el-get-source-name src)))

(provide 'el-get-config)
;;; el-get-config.el ends here

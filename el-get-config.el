;;; el-get-config.el --- Configuration for the el-get package manager
;;
;; Requires git clone git@github.com:sirech/el-get.git

(add-to-loadpath (concat dotfiles-dir "el-get") t)

(require 'el-get)
(setq el-get-dir (concat dotfiles-dir "el-get-packages"))
(setq el-get-verbose t)

;; PACKAGES LIST

(setq packages-modes
      '(
        coffee-mode
        crontab-mode
        css-mode
        csv-mode
        eproject
        feature-mode
        haskell-mode
        js2-mode
        lua-mode
        markdown-mode
        yaml-mode))

(setq packages-git
      '(
        git-commit-mode
        magit
        mo-git-blame
        ))

(setq packages-ruby
      '(
        inf-ruby
        ruby-compilation
        ruby-electric
        ruby-mode
        rspec-mode
        yari
        rhtml-mode
        rbenv
        ))

(setq packages-utilities
      '(
        full-ack
        smex
        ))

(setq packages-theme
      '(
        color-theme
        color-theme-solarized
        powerline
        ))

(setq packages-other
      '(
        auto-complete
        browse-kill-ring
        dired+
        dired-single
        etags-select
        expand-region
        fringe-helper
        htmlize
        ibuffer-vc
        mark-multiple
        pos-tip
        ))

;; With initialization
(setq el-get-sources
      '(
        (:name auto-complete
               :after (progn
                        (require 'auto-complete)
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

        (:name browse-kill-ring
               :after (progn
                        (require 'browse-kill-ring)
                        (browse-kill-ring-default-keybindings)))

        (:name dired-single
               :after (progn
                        (eval-after-load 'dired
                          '(progn
                             (define-key dired-mode-map [return] 'joc-dired-single-buffer)
                             (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
                             (define-key dired-mode-map "^"
                               '(progn
                                  (interactive)
                                  (joc-dired-single-buffer "..")))))))

        (:name git-commit-mode
               :after (progn
                        (add-hook 'git-commit-mode-hook 'run-coding-hook)
                        (add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . git-commit-mode))))

        (:name mark-multiple
               :after (progn
                        (require 'inline-string-rectangle)
                        (global-set-key (kbd "C-x r t") 'inline-string-rectangle)

                        (require 'mark-more-like-this)
                        (global-set-key (kbd "C-<") 'mark-previous-like-this)
                        (global-set-key (kbd "C->") 'mark-next-like-this)))

        (:name paredit
               :after (progn
                        (require 'paredit)
                        (define-key paredit-mode-map (kbd "M-s") 'nil)
                        (define-key paredit-mode-map (kbd "M-r") 'nil)))

        (:name yari
               :after (progn
                        (eval-after-load 'ruby-mode
                          '(define-key ruby-mode-map [f1] 'yari))))

        (:name rbenv
               :after (progn
                        (rbenv--setup)
                        (rbenv-use-global)))

        (:name expand-region
               :after (progn
                        (global-set-key (kbd "C-=") 'er/expand-region)))
        ))

(el-get 'sync
        packages-modes
        packages-ruby
        packages-git
        packages-utilities
        packages-theme
        packages-other
        (loop for src in el-get-sources collect (el-get-source-name src)))

(provide 'el-get-config)
;;; el-get-config.el ends here

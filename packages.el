(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-user-dir (expand-file-name "elpa" dotfiles-dir))

(package-initialize)

(setq packages-modes
      '(
        coffee-mode
        crontab-mode
        css-mode
        csv-mode
        feature-mode
        haml-mode
        haskell-mode
        js2-mode
        json-mode
        lua-mode
        markdown-mode
        puppet-mode
        sass-mode
        yaml-mode))

(setq packages-git
      '(
        git-commit-mode
        ;; Might fail with makeinfo: command not found
        ;; - ubuntu: apt-get install texinfo
        magit
        git-rebase-mode
        mo-git-blame
        ))

(setq packages-ruby
      '(
        inf-ruby
        ruby-compilation
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
        ;; TODO: stop using custom version when changes are somehow
        ;; integrated
        ;; etags-select
        expand-region
        flycheck
        flycheck-color-mode-line
        fringe-helper
        htmlize
        ibuffer-vc
        mark-multiple
        pos-tip
        projectile
        pkg-info ;; needed for projectile
        yasnippet
        ))

(defvar prelude-packages
  (append
   packages-modes
   packages-ruby
   packages-git
   packages-utilities
   packages-theme
   packages-other)
  "A list of packages to ensure are installed at launch."
  )
;; (defvar prelude-packages
;;   '(ace-jump-mode ack-and-a-half anzu
;;     browse-kill-ring
;;     dash diminish elisp-slime-nav
;;     epl expand-region flycheck gist
;;     gitconfig-mode gitignore-mode grizzl
;;     guru-mode projectile
;;     magit move-text rainbow-mode
;;     smartparens undo-tree
;;     volatile-highlights zenburn-theme)
;;   "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  "Check if all packages in `prelude-packages' are installed."
  (every #'package-installed-p prelude-packages))

(defun prelude-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package prelude-packages)
    (add-to-list 'prelude-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun prelude-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'prelude-require-package packages))

(define-obsolete-function-alias 'prelude-ensure-module-deps 'prelude-require-packages)

(defun prelude-install-packages ()
  "Install all packages listed in `prelude-packages'."
  (unless (prelude-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (prelude-require-packages prelude-packages)))

;; run package installation
(prelude-install-packages)

(defun prelude-list-foreign-packages ()
  "Browse third-party packages not bundled with Prelude.

Behaves similarly to `package-list-packages', but shows only the packages that
are installed and are not in `prelude-packages'.  Useful for
removing unwanted packages."
  (interactive)
  (package-show-package-list
   (set-difference package-activated-list prelude-packages)))

(defmacro prelude-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar prelude-auto-install-alist
  '(("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode2 scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)))

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (prelude-auto-install extension package mode))))
 prelude-auto-install-alist)

(provide 'packages)
;;; packages.el ends here

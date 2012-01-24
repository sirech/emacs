;;; init.el --- Where all the magic begins
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"


;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Load path etc.
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(load-file (concat dotfiles-dir "helpers.el"))

(add-to-loadpath dotfiles-dir)
(add-to-loadpath (concat dotfiles-dir "lang"))
(add-to-loadpath (concat dotfiles-dir "vendor") t)

(setq custom-file (concat dotfiles-dir "custom.el"))

;; Library directory where external libraries can be stored
(setq lib-dir (concat dotfiles-dir "lib/"))

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; backport some functionality to Emacs 22 if needed
(require 'dominating-file)

;; Load up el-get, the package manager
(require 'el-get-config)

;; Load up customizations
(require 'defuns)
(require 'bindings)
(require 'server) ; load before misc to avoid problem in windows
(require 'theme)
(require 'mode-line-config)
(require 'ibuffer-config)
(require 'git-config)
(require 'misc)
(require 'registers)
(require 'eshell-utils)
(require 'shell-utils)
(require 'flymake-config)

(load custom-file 'noerror)

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir "machines/" system-name ".el")
      os-specific-config (concat dotfiles-dir "os/" (get-os) ".el")
      user-specific-config (concat dotfiles-dir "users/" user-login-name ".el")
      user-specific-dir (concat dotfiles-dir "users/" user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p os-specific-config) (load os-specific-config))
(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
    (mapc #'load (directory-files user-specific-dir nil ".*el$")))

;; Load language modes, or modes that might need path adjustments for each machine
;; (require 'starter-kit-perl)
(require 'lang-lisp)
(require 'lang-js)
(require 'lang-python)
(require 'lang-xml)
(require 'lang-lua)
(require 'lang-java)
(require 'lang-ruby)
(require 'lang-bash)
(require 'w3m-config)

(require 'desktop-config)
;;; init.el ends here

;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
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

(defun add-to-loadpath (base &optional add-subdirs)
  (add-to-list 'load-path base)
  (if add-subdirs
      (dolist (f (directory-files base))
        (let ( (name (concat base "/" f)) )
          (when (and (file-directory-p name)
                     (not (string= "." (substring f 0 1))))
            (add-to-list 'load-path name))))))

(add-to-loadpath dotfiles-dir)
(add-to-loadpath (concat dotfiles-dir "lang"))
(add-to-loadpath (concat dotfiles-dir "elpa-to-submit"))
(add-to-loadpath (concat dotfiles-dir "vendor") t)

;; Sub-Subdirs have to be loaded manually ...
(add-to-list 'load-path (concat dotfiles-dir "vendor/jdee/lisp"))
(add-to-list 'load-path (concat dotfiles-dir "vendor/cedet/common"))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
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
(require 'bm)
(require 'psvn)

;; backport some functionality to Emacs 22 if needed
(require 'dominating-file)

;; Load up ELPA, the package manager
(require 'package)
(package-initialize)
(require 'starter-kit-elpa)

;; Define variables to enable/disable specific modes
(defvar nxhtml-enabled nil
  "Variable that controls the activation of the nxhtml mode")

(defvar yasnippet-enabled nil
  "Variable that controls the activation of the yasnippet mode")

;; Load up customizations

(require 'defuns)
(require 'bindings)
(require 'server) ; load before misc to avoid problem in windows
(require 'misc)
(require 'registers)
(require 'eshell-utils)
(require 'shell-utils)
(require 'ac-config)
(require 'flymake-config)

(regen-autoloads)
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
;; (require 'starter-kit-lisp)
;; (require 'starter-kit-perl)
(require 'lang-js)
(require 'lang-python)
(require 'lang-xml)
(require 'lang-lua)
(require 'lang-java)
(require 'lang-ruby)
(require 'w3m-config)

;; Load optional modes
(when nxhtml-enabled
  (load "elpa-to-submit/nxhtml/autostart"))

(when yasnippet-enabled  
  (require 'yasnippet)
  (yas/initialize)
  (yas/load-directory (concat dotfiles-dir "/vendor/yasnippet/snippets"))
  ;; (yas/load-directory yas/root-directory)
  (yas/minor-mode-on)
  (add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand)
  (setq yas/prompt-functions '(yas/x-prompt yas/dropdown-prompt))
  )

;;; init.el ends here

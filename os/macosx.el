;;; macosx.el --- Specific settings for Mac OS X

;; Stupid OS X uses a crappy path. We've got to set it manually. Twice.
(add-to-path "/usr/local/bin" "/opt/local/bin")

;; Also have to export env variables
(setenv "JAVA_HOME" "/Library/Java/Home")
(setenv "ANT_HOME" "/usr/share/ant")

;; Remap meta and command
(setq mac-command-modifier (quote control))
(setq mac-control-modifier (quote meta))

;; Stop C-h from hiding
(setq mac-pass-command-to-system nil)
(setq mac-pass-control-to-system nil)

;; Set default browser
(setq browse-url-browser-function 'browse-default-macosx-browser)

;; Add java VM
(setq jde-jdk-registry
      '(("1.6" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.6")
        ("1.5" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.5")))
(setq jde-jdk "1.6")

;; Ignore .DS_Store files
(eval-after-load 'ido-mode
  '(progn
     (add-to-list 'ido-ignore-files "\\.DS_Store")))

;; Change focus automatically when emacsclient is used to edit a file.
;; Credit to
;; http://stackoverflow.com/questions/945709/emacs-23-os-x-multi-tty-and-emacsclient

;; Switch to the given app
(defun ns-switch-to (app)
  (ns-do-applescript (concat "tell application \"" app "\" to activate")))

(defun ns-raise-emacs ()
  (ns-switch-to "Emacs"))

(defun ns-raise-terminal ()
  (ns-switch-to "Terminal"))

(defun ns-raise-iterm ()
  (ns-switch-to "iTerm"))

(defun ns-raise-chrome ()
  (ns-switch-to "Google Chrome"))

(add-hook 'server-visit-hook 'ns-raise-emacs)
(add-hook 'server-done-hook 'ns-raise-iterm)

(add-hook 'edit-server-start-hook 'ns-raise-emacs)
(add-hook 'edit-server-done-hook 'ns-raise-chrome)

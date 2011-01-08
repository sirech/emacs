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
(defun ns-raise-emacs ()
  (ns-do-applescript "tell application \"Emacs\" to activate"))
(defun end-server-edit ()
  (ns-do-applescript "tell application \"Terminal\" to activate"))
(add-hook 'server-visit-hook 'ns-raise-emacs)
(add-hook 'server-done-hook 'end-server-edit)

;;; macosx.el --- Specific settings for Mac OS X

; Stupid OS X uses a crappy path. We've got to set it manually. Twice.
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


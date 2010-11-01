;;; finks.el --- Settings specific to a machine

;; (set-frame-size-according-to-resolution)
(setq initial-frame-alist
      '((left . 0) (top . 0)
	(width . 329) (height . 111)))

;; Export env variables
(setenv "WORKSPACE" "~/Documents/workspace")
(setenv "RSENSE_HOME" "/opt/local/share/rsense-0.3")

;; Use inconsolata font
(set-face-attribute 'default nil :family "Inconsolata" :height 160)

;; Enable YASnippet
(setq yasnippet-enabled t)

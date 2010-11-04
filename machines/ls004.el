;;; ls004.el --- Settings specific to a machine

;; (set-frame-size-according-to-resolution)
(setq initial-frame-alist
      '((left . 0) (top . 0)
	(width . 158) (height . 55)))

;; Export env variables
(setenv "RSENSE_HOME" "/opt/rsense")

;; Use inconsolata font
(set-face-attribute 'default nil :family "Inconsolata" :height 120)

;; setup proxy
(setq url-using-proxy t)
(setq url-proxy-services
      '(("http" . "10.0.175.40:3128")))

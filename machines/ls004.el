;;; ls004.el --- Settings specific to a machine

;; (set-frame-size-according-to-resolution)
(setq initial-frame-alist
      '((left . 0) (top . 0)
        (width . 160) (height . 58)))

;; Export env variables
(setenv "RSENSE_HOME" "/opt/rsense")
(setenv "GIT_PROXY_COMMAND" "git-proxy-cmd")
(setenv "PROXY_HOST" "10.0.175.40")

;; Use inconsolata font
(set-face-attribute 'default nil :family "Inconsolata" :height 120)

;; setup proxy
(setq url-using-proxy t)
(setq url-proxy-services
      '(("http" . "10.0.175.40:3128")))

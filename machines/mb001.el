;;; mb001.el --- Settings specific to a machine

;; Export env variables
(setenv "GIT_PROXY_COMMAND" "git-proxy-cmd")
(setenv "PROXY_HOST" "10.0.175.40")

;; Use inconsolata font
(set-face-attribute 'default nil :family "Inconsolata" :height 160)

;; setup proxy
(setq url-using-proxy t)
(setq url-proxy-services
      '(("http" . "10.0.175.40:3128")))

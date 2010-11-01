;;; CASTERLYROCK.el --- Settings specific to a machine

;; (set-frame-size-according-to-resolution)
(setq initial-frame-alist
      '((left . 0) (top . 0)
	(width . 236) (height . 63)))

;; Add cygwin to path
(add-to-list 'exec-path "C:/Utilities/cygwin/bin")

;; ;; Use pretty Consolas font, with size 11pt
;; (set-face-attribute 'default nil :family "Consolas" :height 110)

;; Record commands used
(require 'command-frequency)
(command-frequency-table-load)
(command-frequency-mode 1)
(command-frequency-autosave-mode 1)

;; Add .mpx files as xml based
(add-to-list 'auto-mode-alist '("\\.mpx$" . nxml-mode))

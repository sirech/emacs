;;; shalnark.el --- Settings specific to a machine

;; (set-frame-size-according-to-resolution)
(setq initial-frame-alist
      '((left . 0) (top . 0)
	(width . 203) (height . 61)))

;; Use inconsolata font
(set-face-attribute 'default nil :family "Inconsolata" :height 140)

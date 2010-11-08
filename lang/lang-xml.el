;;; lang-xml.el --- Configuration for XML documents

(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))

;; Start hooks
(add-hook 'nxml-mode-hook 'run-coding-hook)
(add-hook 'nxml-mode-hook 'turn-on-autocomplete) 

(eval-after-load 'nxml-mode
  '(progn
     (setq nxml-slash-auto-complete-flag t)))

(provide 'lang-xml)
;;; lang-xml.el ends here

;;; bindings.el --- Set up some handy key bindings

;; Invoke M-x without the alt key
(global-set-key "\C-x\C-m" 'execute-extended-command)
;; (global-set-key "\C-c\C-m" 'execute-extended-command)

;; repeat commands
(global-set-key "\C-x." 'repeat)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;;
;; CLEANUP
;;

;; Use kill word instead of backspace. Remap kill region
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
;; (global-set-key "\C-c\C-k" 'kill-region)

;; Add function to copy single line
(global-set-key "\C-c\C-k" 'copy-line)

;; reload changed buffer
(global-set-key "\M-r" 'revert-buffer)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;;
;; FINDING STUFF
;;

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; YASnippet
;; (eval-after-load 'yasnippet
;;   '(progn
;;      (setq yas/trigger-key (kbd "M-SPC"))))

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; Yank current word into isearch
(define-key isearch-mode-map (kbd "C-x") 'isearch-yank-current-word)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Bookmarks
(global-set-key (kbd "C-<f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "S-<f2>") 'bm-previous)

;;
;; MOVING BETWEEN WINDOWS
;;

;; Moving between paragraphs
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

;;
;; INDENTING / COMMENTS
;;

;; make commenting/uncommenting easy
(global-set-key (kbd "C-;") 'comment-region)
(global-set-key (kbd "C-'") 'uncomment-region)

;; enter indents automatically
(global-set-key (kbd "RET") 'newline-and-indent)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;;
;; RECTANGLE
;;

;; Surround a rectangle from the left and the right
(define-key ctl-x-r-map "u" 'surround-rectangle-with)
;; Insert something before a rectangle
(define-key ctl-x-r-map "p" 'string-insert-rectangle)

;;
;; SHELL
;;

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

;;
;; MISC
;;

;; Fetch the contents at a URL, display it raw.
(global-set-key (kbd "C-x C-h") 'view-url)

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'eval-and-replace)

;; For debugging Emacs modes
(global-set-key (kbd "C-c p") 'message-point)

;; Applications

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(provide 'bindings)
;;; bindings.el ends here

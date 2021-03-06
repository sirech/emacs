;;; ibuffer-config.el --- Configuration for ibuffer
;;

(require 'ibuffer)

;; Custom groups for the buffers
;; (setq ibuffer-saved-filter-groups
;;       (quote (("default"
;;                ("Python"
;;                 (mode . python-mode))
;;                ("Ruby"
;;                 (mode . ruby-mode))
;;                ("Shell Scripting"
;;                 (or
;;                  (mode . shell-script-mode)
;;                  (mode . sh-mode)
;;                  ))
;;                ("XML"
;;                 (mode . nxml-mode))
;;                ("Emacs Configuration"
;;                 (mode . emacs-lisp-mode))
;;                ("*Buffer*"
;;                 (name . "^\\*.*\\*$"))
;;                ))))

;; ;; Add ibuffer-vc grouping to the list of buffers
;; (defun ibuffer-vc-add-vc-filter-groups ()
;;   (interactive)
;;   (dolist (group (ibuffer-vc-generate-filter-groups-by-vc-root))
;;     (add-to-list 'ibuffer-filter-groups group t)))

;; (add-hook 'ibuffer-mode-hook
;;           (lambda ()
;;             (ibuffer-switch-to-saved-filter-groups "default")
;;             (ibuffer-vc-add-vc-filter-groups)))

(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (ibuffer-do-sort-by-alphabetic)))

(eval-after-load 'ibuffer
  '(progn

     (require 'ibuffer-vc)

     ;; Use human readable Size column instead of original one
     (define-ibuffer-column size-h
       (:name "Size" :inline t)
       (cond
        ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
        ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
        (t (format "%8d" (buffer-size)))))

     ))

;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              ;; Readable size
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))

(ad-activate 'ibuffer)

(setq ibuffer-filter-group-name-face 'font-lock-doc-face)

(provide 'ibuffer-config)
;;; ibuffer-config.el ends here

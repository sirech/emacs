;;; flymake-config.el --- Configuration for flymake
;;

;; INITIALIZATION FUNCTIONS

(defun turn-on-flymake ( &optional is-available-fun )
  "Activates flymake. Optionally a function can be passed to check if flymake should be activated"
  (when (or (not is-available-fun) (funcall is-available-fun))
    (require 'flymake)
    (flymake-mode t)))

(defun setup-flymake (is-available-fun extension flymake-init &optional flymake-cleanup line-patterns)
  "Configures flymake. In case the available function returns
non-nill, the files with the given extension are set to use the
given init function. Additionally, one can also give a cleanup
function and a error line patterns variable."
  (let ( (mask (concat "\\." extension "\\'")) )
    (remove-extension-flymake extension)
    (when (or (not is-available-fun) (funcall is-available-fun))
      (if flymake-cleanup
          (add-to-list 'flymake-allowed-file-name-masks
                       (list mask flymake-init flymake-cleanup))
        (add-to-list 'flymake-allowed-file-name-masks
                     (list mask flymake-init)))
      (if line-patterns
          (loop for pattern in line-patterns do (push pattern flymake-err-line-patterns))))))

(defun custom-flymake-init (executable &rest extra-args)
  "Initializes flymake with a custom command, accepting also extra arguments for the command"
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list executable (append extra-args (list local-file)))))

(defun remove-extension-flymake (mask)
  "Removes the given extension from the list that will be used by
  flymake"
  (remove-from-list 'flymake-allowed-file-name-masks
                    (lambda (item) (string= (car item) mask))))

;; Don't require moving mouse to show error, credit to http://www.emacswiki.org/emacs/FlyMake#toc13
(defun my-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

;; SETTINGS

(eval-after-load 'flymake
  '(progn
     (add-hook 'post-command-hook 'my-flymake-show-help)
     (add-hook 'find-file-hook 'flymake-find-file-hook)
     ;; Remove extensions without a working config
     (remove-extension-flymake "\\.xml\\'")
     (remove-extension-flymake "\\.html?\\'")
     (remove-extension-flymake "\\.tex\\'")
     ))

(provide 'flymake-config)
;;; flymake-config.el ends here

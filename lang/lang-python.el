;;; lang-python.el --- Configuration for Python
;;
;; Flycheck:
;;  * pip install flake8
;;
;; IDE:
;;  * pymacs
;;   - goto http://pymacs.progiciels-bpi.ca/ (beta versions don't seem
;; to work)
;;   - download pymacs, untar, run python setup.py install
;;   - put pymacs.el in vendor/
;;  * rope
;;   - hg clone http://bitbucket.org/agr/rope
;;   - hg clone http://bitbucket.org/agr/ropemacs
;;   - hg clone http://bitbucket.org/agr/ropemode
;;   - easy_install rope
;;   - ln -s ../ropemode/ropemode ropemacs/
;;   - easy_install ropemacs
;;
;; The functionality is turned on only if the required dependencies
;; can be found.
;;
;; NOTE: there is no easy way to check if the Pymacs and ropemacs
;; python modules are there. Therefore we only check the presence of
;; the pymacs.el. If it is present, but the python modules are not
;; there, it will not work.

;; Set hooks

(add-hook 'python-mode-hook 'run-coding-hook)

;; Check availability
(defun py-ide-is-present ()
  "Determines if the IDE can be run"
  (locate-library "pymacs"))

;; Activation
(when (py-ide-is-present)
  (ac-ropemacs-initialize))

;; Initialization
(eval-after-load 'ropemacs
  '(progn
     (define-key ropemacs-local-keymap (kbd "M-.") 'rope-goto-definition)
     ))

(provide 'lang-python)
;;; lang-python.el ends here

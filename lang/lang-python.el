;;; lang-python.el --- Configuration for Python
;;
;; This settings have some external dependencies:
;;  * for pyflakes
;;   - easy_install pyflakes
;;  * for pep8
;;   - easy_install pep8
;;  * for pymacs
;;   - goto http://pymacs.progiciels-bpi.ca/ (beta versions don't seem
;; to work)
;;   - download pymacs, untar, run python setup.py install
;;   - put pymacs.el in vendor/
;;  * for rope
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
(add-hook 'python-mode-hook 'py-activate-ide)

;; Check availability

(defun pyflakes-is-present ()
  "Determines if the syntax checker is present in the system"
  (executable-find "pyflakes"))

(defun pep8-is-present ()
  "Determines if the program that checks if a python script
complies with the PEP8 (code style guide) is present in the
system"
  (executable-find "pep8"))

(defun py-ide-is-present ()
  "Determines if the IDE can be run"
  (locate-library "pymacs"))

;; Activation

(defun py-activate-ide ()
  (turn-on-flymake 'pyflakes-is-present)
  (when (py-ide-is-present)
    (py-activate-rope)))

(defun py-activate-rope ()
  (require 'auto-complete-config)
  (add-to-list 'ac-sources 'ac-source-ropemacs)
  (setq ac-auto-start nil))
  ;; (set (make-local-variable 'ac-sources)
  ;;      '(ac-source-rope));;DISABLE YASNIPPET (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))

;; Initialization

;; Remap checker to pep8
(eval-after-load 'python
  '(when (pep8-is-present)
     (setq python-check-command "pep8 --repeat")))

;; Pymacs
(when (py-ide-is-present)
  (autoload 'pymacs-apply "pymacs")
  (autoload 'pymacs-call "pymacs")
  (autoload 'pymacs-eval "pymacs" nil t)
  (autoload 'pymacs-exec "pymacs" nil t)
  (autoload 'pymacs-load "pymacs" nil t))

;; Rope
(defun load-rope ()
  (setq ropemacs-enable-shortcuts nil)
  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-enable-autoimport t))

(eval-after-load 'python
  '(progn
     (when (py-ide-is-present)
       (load-rope))


     ;;
     ;; KEYBINDINGS
     ;;

     ;; VS-style compile controls
     (define-key python-mode-map (kbd "<f7>") 'python-check-command)
     ))

(eval-after-load 'ropemacs
  '(progn
     (define-key ropemacs-local-keymap (kbd "M-.") 'rope-goto-definition)
     ))

(eval-after-load 'flymake
  '(setup-flymake 'pyflakes-is-present "py" 'flymake-pyflakes-init))

;; Auto Syntax Error Highlight, credit to http://hide1713.wordpress.com/2009/01/30/setup-perfect-python-environment-in-emacs/

(defun flymake-pyflakes-init ()
  (custom-flymake-init "pyflakes"))

(provide 'lang-python)
;;; lang-python.el ends here

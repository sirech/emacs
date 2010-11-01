;;; lang-python.el --- Configuration for Python
;;
;; This settings have some external dependencies:
;;  * for pyflakes
;;   - easy_install pyflakes
;;  * for pep8
;;   - easy_install pep8
;;  * for pymacs
;;   - goto http://pymacs.progiciels-bpi.ca/
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
  (turn-on-autocomplete)
  (when (py-ide-is-present)
    (py-activate-rope)))

(defun py-activate-rope ()
  (require 'auto-complete-config)
  (add-to-list 'ac-sources 'ac-source-ropemacs)
  (setq ac-auto-start nil))
  ;; (set (make-local-variable 'ac-sources)
  ;;      '(ac-source-rope));;DISABLE YASNIPPET (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
  ;; (set (make-local-variable 'ac-find-function) 'ac-python-find)
  ;; (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate))


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
     (define-key python-mode-map (kbd "<f7>") 'python-check-command)))

(eval-after-load 'flymake
  '(setup-flymake 'pyflakes-is-present "py" 'flymake-pyflakes-init))

;; Auto Syntax Error Highlight, credit to http://hide1713.wordpress.com/2009/01/30/setup-perfect-python-environment-in-emacs/

(defun flymake-pyflakes-init ()
  (custom-flymake-init "pyflakes"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-completion
;;;  Integrates:
;;;   1) Rope
;;;   2) Yasnippet
;;;   all with AutoComplete.el
;;; Credit to http://www.enigmacurry.com/2009/01/21/autocompleteel-python-code-completion-in-emacs/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun prefix-list-elements (list prefix)
;;   (let (value)
;;     (nreverse
;;      (dolist (element list value)
;;        (print (format "%s%s" prefix element))
;;        (setq value (cons (format "%s%s" prefix element) value))))))

;; (defvar ac-source-rope
;;   '((candidates
;;      . (lambda ()
;;          (prefix-list-elements (rope-completions) ac-target))))
;;   "Source for Rope")

;; (defun ac-python-find ()
;;   "Python `ac-find-function'."
;;   (require 'thingatpt)
;;   (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
;;     (if (null symbol)
;;         (if (string= "." (buffer-substring (- (point) 1) (point)))
;;             (point)
;;           nil)
;;       symbol)))

;; (defun ac-python-candidate ()
;;   "Python `ac-candidates-function'"
;;   (let (candidates)
;;     (dolist (source ac-sources)
;;       (if (symbolp source)
;;           (setq source (symbol-value source)))
;;       (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
;;              (requires (cdr-safe (assq 'requires source)))
;;              cand)
;;         (if (or (null requires)
;;                 (>= (length ac-target) requires))
;;             (setq cand
;;                   (delq nil
;;                         (mapcar (lambda (candidate)
;;                                   (propertize candidate 'source source))
;;                                 (funcall (cdr (assq 'candidates source)))))))
;;         (if (and (> ac-limit 1)
;;                  (> (length cand) ac-limit))
;;             (setcdr (nthcdr (1- ac-limit) cand) nil))
;;         (setq candidates (append candidates cand))))
;;     (delete-dups candidates)))

;; (defadvice ac-start (before advice-turn-on-auto-start activate)
;;   (set (make-local-variable 'ac-auto-start) t))
;; (defadvice ac-cleanup (after advice-turn-off-auto-start activate)
;;   (set (make-local-variable 'ac-auto-start) nil))

;;; END AUTOCOMPLETION

(provide 'lang-python)
;;; lang-python.el ends here

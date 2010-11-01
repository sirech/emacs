;;; lang-java.el --- Configuration for Java
;;
;; This settings have some external dependencies:
;;  * jdee: Install the package in the vendor dir.
;;
;;  * cedet: Install the package in the vendor dir.
;;
;;  * eclipse batch compiler (ecj.jar):
;;   - http://download.eclipse.org/eclipse/downloads/, choose release, go to JDT Core batch compiler
;;   - put the jar file in the lib directory, renaming it to ecj.jar (remove version number)
;;
;; The functionality is turned on only if the required
;; dependencies can be found.

;; Set hooks

(add-hook 'jde-mode-hook 'run-coding-hook)
(add-hook 'jde-mode-hook 'jde-activate-ide)

;; Check availability

(defvar ecj-location (concat lib-dir "ecj.jar")
  "The location of the ecj compiler")

(defun jde-ide-is-present ()
  "Determines if the IDE can be run"
  (and
   (locate-library "cedet")
   (locate-library "jde")))

(defun ecj-is-present ()
  "Determines if the syntax checker is present in the system"
  (and
   (jde-ide-is-present)
   (file-exists-p ecj-location)))

;; Attach mode

;; Don't do this if jde-mode is not there
(when (jde-ide-is-present)
  (add-to-list 'auto-mode-alist '("\\.java$" . jde-mode)))

;; Activation

(defun jde-activate-ide ()
  (turn-on-flymake 'ecj-is-present)
  (turn-on-autocomplete)
  (when (jde-ide-is-present)
    (jde-activate-autocomplete)))

(defun jde-activate-autocomplete ()
  (add-to-list 'ac-sources 'ac-source-jde))

;; Initialization

(when (jde-ide-is-present)
  (require 'cedet)
  (autoload 'jde-mode "jde" "JDE mode." t))

;; TODO: this triggers the loading of jde and flymake
(when (ecj-is-present)
  (require 'jde-eclipse-compiler-server)
  (setq jde-compiler (list (list "eclipse java compiler server" ecj-location)))
  (setq jde-ecj-command-line-args (list "-d" "none" "-target" "1.6" "-source" "1.6" "-proceedOnError")))

(eval-after-load 'jde-mode
  '(progn
     (setq jde-ant-enable-find t)
     (setq jde-ant-home "$ANT_HOME")
     (setq jde-auto-parse-max-buffer-size 0)
     (setq jde-build-function '(jde-ant-build))
     (setq jde-complete-function 'jde-complete-minibuf)
     (setq jde-compile-option-source '("1.6"))
     (setq jde-electric-return-p t)

     ;;
     ;; KEYBINDINGS
     ;;

     ;; VS-style compile controls
     (define-key jde-mode-map (kbd "<f7>") 'jde-compile)))

;; Change flymake to use the ecj
(eval-after-load 'flymake
  '(setup-flymake 'ecj-is-present "java" 'jde-ecj-flymake-init 'jde-ecj-flymake-cleanup nil))

;; Auto-completion

(defvar ac-source-jde
  '((candidates
     . (lambda ()
         (mapcar 'cdr (jde-autocomplete)))))
  "Source for JDE")

(defun jde-autocomplete ()
  (interactive)
  (let* ((pair (jde-parse-java-variable-at-point))
         jde-parse-attempted-to-import)
    ;;resetting jde-complete-current-list
    (setq jde-complete-current-list nil)
    (if pair
        (condition-case err
            (jde-autocomplete-pair (jde-complete-get-pair pair nil))
          (error (condition-case err
                     (jde-autocomplete-pair (jde-complete-get-pair pair t)))
                 (error (message "%s" (error-message-string err)))))
      (message "No completion at this point"))))

(defun jde-autocomplete-pair (pair)
  (let ((access (jde-complete-get-access pair))
        completion-list)
    (progn
      (if access
          (setq completion-list
                (jde-complete-find-completion-for-pair pair nil access))
        (setq completion-list (jde-complete-find-completion-for-pair pair)))
      ;;if the completion list is nil check if the method is in the current
      ;;class(this)
      (if (null completion-list)
          (setq completion-list (jde-complete-find-completion-for-pair
                                 (list (concat "this." (car pair)) "")
                                 nil jde-complete-private)))
      ;;if completions is still null check if the method is in the
      ;;super class
      (if (null completion-list)
          (setq completion-list (jde-complete-find-completion-for-pair
                                 (list (concat "super." (car pair)) "")
                                 nil jde-complete-protected)))

      (if completion-list
          jde-complete-current-list
        (error "No completion at this point")))))

;; (defun my-jde-mode-hook ()
;;   "Hook for running Java file..."
;;   (message "Loading my-java-hook...")
;;   (define-key c-mode-base-map "\C-ca" 'jde-javadoc-generate-javadoc-template)
;;   (define-key c-mode-base-map "\C-m" 'newline-and-indent)
;;   (c-set-offset 'substatement-open 0)
;;   (c-set-offset 'statement-case-open 0)
;;   (c-set-offset 'case-label '+)
;;   (fset 'my-javadoc-code
;;      [?< ?c ?o ?d ?e ?>?< ?/ ?c ?o ?d ?e ?> left left left left left left left])
;;   (define-key c-mode-base-map "\C-cx" 'my-javadoc-code)
;;   (abbrev-mode t)
;;   (setq c-comment-continuation-stars "* "
;;      tab-width 4
;;      indent-tabs-mode nil
;;      tempo-interactive t
;;      c-basic-offset 4)
;;   (message "my-jde-mode-hook function executed")
;; )

;; (add-hook 'jde-mode-hook 'my-jde-mode-hook)
;; (add-hook 'jde-mode-hook 'load-cedet)

;; (eval-after-load 'jde-mode
;;   (load "vendor/cedet/common/cedet.el"))

(provide 'lang-java)
;;; lang-java.el ends here

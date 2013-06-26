;;; lang-java.el --- Configuration for Java
;;
;; This settings have some external dependencies:
;;  * jdee: Install the package in the vendor dir.
;;
;;  * cedet: Install the package in the vendor dir.
;;
;; Alternative guide:
;;  * eclipse:
;;   - http://eclipse.org/downloads/index.php
;;
;;  * eclim:
;;   - http://eclim.org/download.html
;;   - run the jar file, which launches the installation
;; automatically.
;;   - eclimd, under ${ECLIPSE_DIR}/eclimd has to be running.
;;   - put eclim in the path, with something like: ln -s
;; ${eclipse_dir}/plugins/org.eclim_${version}/bin/eclim /usr/local/bin
;;
;;  * emacs-eclim:
;;   - https://github.com/senny/emacs-eclim

;; Set hooks

(add-hook 'java-mode-hook 'run-coding-hook)
(add-hook 'java-mode-hook 'java-activate-ide)
;; (add-hook 'jde-mode-hook 'run-coding-hook)
(add-hook 'jde-mode-hook 'jde-activate-ide)

;; Check availability
(defun eclim-is-present ()
  "Determines if the IDE can be run"
  (locate-library "eclim"))

(defun jde-ide-is-present ()
  "Determines if the IDE can be run"
  (and
   (locate-library "cedet")
   (locate-library "jde")))

;; Attach mode

;; Don't do this if jde-mode is not there
(when (jde-ide-is-present)
  (add-to-list 'auto-mode-alist '("\\.java$" . jde-mode)))

;; Activation

(defun java-activate-ide ()
  (when (eclim-is-present)
    (java-activate-autocomplete)))

(defun java-activate-autocomplete ()
  (require 'auto-complete-config)
  (add-to-list 'ac-sources 'ac-source-eclim))

;; Initialization

(when (jde-ide-is-present)
  (require 'cedet)
  (autoload 'jde-mode "jde" "JDE mode." t))

(defun load-eclim ()
  (require 'eclim)
  (setq eclim-auto-save t)
  (global-eclim-mode))

(eval-after-load 'java
  (progn
    (when (eclim-is-present)
      (load-eclim))))

(eval-after-load 'eclim
  '(progn
     (define-key eclim-mode-map (kbd "M-.") 'eclim-java-find-declaration)
     ))

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

(provide 'lang-java)
;;; lang-java.el ends here

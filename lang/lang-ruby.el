;;; lang-ruby.el --- Configuration for Ruby
;;
;; This settings have some external dependencies:
;;  * rsense:
;;   - http://cx4a.org/software/rsense/index.html
;;   - extract the file (e.g /opt)
;;    - set the env variable RSENSE_HOME (in emacs!) with the path
;;    - add permissions with 'chmod +x bin/rsense'
;;   - create config file with 'ruby etc/config.rb > ~/.rsense' (must be IN RSENSE_HOME)
;;   - copy the rsense.el file in /etc to the /vendor directory in emacs
;;
;; NOTE: As of today (11/2010), it is not possible to use RSense with JRuby

;; Set hooks

(add-hook 'ruby-mode-hook 'run-coding-hook)
(add-hook 'ruby-mode-hook 'ruby-activate-ide)

;; Check availability
(setq rsense-home (getenv "RSENSE_HOME"))

(defun ruby-is-present ()
  "Determines whether ruby is installed in the system"
  (executable-find "ruby"))

(defun ruby-ide-is-present ()
  "Determines if the IDE can be run"
  (and
   (locate-library "rsense")
   (executable-find (concat rsense-home "/bin/rsense"))))

;; Attach mode

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;; Activation

(defun ruby-activate-ide ()
  (turn-on-flymake 'ruby-is-present)
  (ruby-activate-autocomplete)
  (when (ruby-ide-is-present)
    (ruby-activate-rsense)))

(defun ruby-activate-autocomplete ()
  (require 'auto-complete-config)
  (turn-on-autocomplete)
  (ac-ruby-mode-setup))

(defun ruby-activate-rsense ()
  (require 'rsense)
  (add-to-list 'ac-sources 'ac-source-rsense-method))

;; Initialization

(eval-after-load 'ruby-mode
  '(progn
     ;; work around possible elpa bug
     (ignore-errors (require 'ruby-compilation))
     ;; (setq ruby-use-encoding-map nil)
     ;; (add-hook 'ruby-mode-hook 'inf-ruby-keys)
     ;; (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     ;; (define-key ruby-mode-map (kbd "C-M-h") 'backward-kill-word)
     (define-key ruby-mode-map (kbd "M-.") 'rsense-jump-to-definition)
     (define-key ruby-mode-map (kbd "C-c l") "lambda")))

;; (global-set-key (kbd "C-h r") 'ri)

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

;; If rvm is used, ruby is not in the path and emacs can't find it
(require 'rvm)
(rvm-autodetect-ruby)

;;; Rake

(defun pcomplete/rake ()
  "Completion rules for the `ssh' command."
  (pcomplete-here (pcmpl-rake-tasks)))

(defun pcmpl-rake-tasks ()
  "Return a list of all the rake tasks defined in the current
projects.  I know this is a hack to put all the logic in the
exec-to-string command, but it works and seems fast"
  (delq nil (mapcar '(lambda(line)
                       (if (string-match "rake \\([^ ]+\\)" line) (match-string 1 line)))
                    (split-string (shell-command-to-string "rake -T") "[\n]"))))

(defun rake (task)
  (interactive (list (completing-read "Rake (default: default): "
                                      (pcmpl-rake-tasks))))
  (shell-command-to-string (concat "rake " (if (= 0 (length task)) "default" task))))


;; Clear the compilation buffer between test runs.
(eval-after-load 'ruby-compilation
  '(progn
     (defadvice ruby-do-run-w/compilation (before kill-buffer (name cmdlist))
       (let ((comp-buffer-name (format "*%s*" name)))
         (when (get-buffer comp-buffer-name)
           (with-current-buffer comp-buffer-name
             (delete-region (point-min) (point-max))))))
     (ad-activate 'ruby-do-run-w/compilation)))

(defvar flymake-ruby-err-line-patterns '(("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$"
                                          1 2 nil 3)))

(eval-after-load 'flymake
  '(setup-flymake 'ruby-is-present "rb" 'flymake-ruby-init nil flymake-ruby-err-line-patterns))

;; Auto Syntax Error highlight, credit to the starter kit from www.peepcode.com

(defun flymake-ruby-init ()
  (custom-flymake-init "ruby" "-c" "-W2"))

;; Rinari (Minor Mode for Ruby On Rails)
;; (setq rinari-major-modes
;;       (list 'mumamo-after-change-major-mode-hook 'dired-mode-hook 'ruby-mode-hook
;;             'css-mode-hook 'yaml-mode-hook 'javascript-mode-hook))

;; TODO: set up ri
;; TODO: electric

(provide 'lang-ruby)
;; lang-ruby.el ends here

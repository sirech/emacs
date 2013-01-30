;;; lang-ruby.el --- Configuration for Ruby
;;
;; This settings have some external dependencies:
;;
;; To use auto completion you can use either rcodetools or
;; rsense. RSense seems to be more powerful, but it does not work very
;; well with JRuby and is more difficult to install. Usually one of
;; both should suffice.
;;
;;  * rcodetools:
;;   - gem install rcodetools
;;   - copy rcodetools.el in the gem file to vendor/
;;
;;  * rsense:
;;   - http://cx4a.org/software/rsense/index.html
;;   - extract the file (e.g /opt)
;;    - set the env variable RSENSE_HOME (in emacs!) with the path
;;    - add permissions with 'chmod +x bin/rsense'
;;   - create config file with 'ruby etc/config.rb > ~/.rsense' (must be IN RSENSE_HOME)
;;   - copy the rsense.el file in /etc to the /vendor directory in emacs
;;
;;  * ruby:
;;   - Don't use JRuby for the global ruby, like if you use rbenv
;;
;; NOTE: As of today (11/2010), it is not possible to use RSense with JRuby

;; Set hooks

(add-hook 'ruby-mode-hook 'run-coding-hook)
(add-hook 'ruby-mode-hook 'ruby-activate-ide)
(add-hook ‘ruby-mode-hook (lambda () (add-to-list ‘write-file-functions ‘delete-trailing-whitespace)))

;; Add rbenv to path
(add-to-path "~/.rbenv/shims" "~/.rbenv/bin")

;; Check availability
(setq rsense-home (getenv "RSENSE_HOME"))

(defun ruby-is-present ()
  "Determines whether ruby is installed in the system"
  ruby-binary)

(defun rcodetools-is-present ()
  "Determines whether rcodetools is installed"
  (locate-library "rcodetools"))

(defun rsense-is-present ()
  "Determines if rsense can be run"
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
  (when (rsense-is-present)
    (ruby-activate-rsense))
  (when (rcodetools-is-present)
    (ruby-activate-rcodetools)))

(defun ruby-activate-rsense ()
  (require 'rsense)
  (add-to-list 'ac-sources 'ac-source-rsense-method))

(defun ruby-activate-rcodetools ()
  (require 'auto-complete-ruby)
  (ac-ruby-setup)
  (auto-complete-mode t))

;; Initialization

(defvar ruby-binary
  (cond
   ((executable-find "ruby") "ruby")
   (t nil))
  "The ruby binary (not JRuby!)")

(eval-after-load 'ruby-mode
  '(progn

     (setq ruby-deep-indent-paren nil)
     
     ;; Indent things like :after_save properly
     (defadvice ruby-indent-line (after line-up-args activate)
       (let (indent prev-indent arg-indent)
         (save-excursion
           (back-to-indentation)
           (when (zerop (car (syntax-ppss)))
             (setq indent (current-column))
             (skip-chars-backward " \t\n")
             (when (eq ?, (char-before))
               (ruby-backward-sexp)
               (back-to-indentation)
               (setq prev-indent (current-column))
               (skip-syntax-forward "w_.")
               (skip-chars-forward " ")
               (setq arg-indent (current-column)))))
         (when prev-indent
           (let ((offset (- (current-column) indent)))
             (cond ((< indent prev-indent)
                    (indent-line-to prev-indent))
                   ((= indent prev-indent)
                    (indent-line-to arg-indent)))
             (when (> offset 0) (forward-char offset))))))

     ;; Align paren at the end at the beginning of the line
     (defadvice ruby-indent-line (after unindent-closing-paren activate)
       (let ((column (current-column))
             indent offset)
         (save-excursion
           (back-to-indentation)
           (let ((state (syntax-ppss)))
             (setq offset (- column (current-column)))
             (when (and (eq (char-after) ?\))
                        (not (zerop (car state))))
               (goto-char (cadr state))
               (setq indent (current-indentation)))))
         (when indent
           (indent-line-to indent)
           (when (> offset 0) (forward-char offset)))))

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

;;; Cucumber

(eval-after-load 'ruby-mode
  '(progn
     (add-to-list 'ruby-font-lock-syntactic-keywords
                  '("\\(\\(\\)\\(\\)\\|Given\\|When\\|Then\\)\\s *\\(/\\)[^/\n\\\\]*\\(\\\\.[^/\n\\\\]*\\)*\\(/\\)"
                    (4 (7 . ?/))
                    (6 (7 . ?/))))))

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
  (custom-flymake-init ruby-binary "-c" "-W2"))

;; Rinari (Minor Mode for Ruby On Rails)
;; (setq rinari-major-modes
;;       (list 'mumamo-after-change-major-mode-hook 'dired-mode-hook 'ruby-mode-hook
;;             'css-mode-hook 'yaml-mode-hook 'javascript-mode-hook))

;; TODO: set up ri
;; TODO: electric

(provide 'lang-ruby)
;; lang-ruby.el ends here

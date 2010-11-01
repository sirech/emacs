;;; w3m-config.el --- Configuration for the w3m browser
;;
;; This settings have some external dependencies:
;;  * w3m: The w3m browser has to be installed and be accessible from emacs
;;    - cygwin: The package is in cygwin, but not included by default.
;;              cygwin/bin has to be added to the exec-path
;;
;;  * emacs-w3m: Install the package in the vendor dir.
;;               Warning: emacs23 needs the one from the CVS (as of 02/2010).
;;
;; The functionality is turned on only if the required
;; dependencies can be found.

;; Check availability

(defun w3m-is-present ()
  "Determines if the w3m browser is present in the system"
  (and
   (executable-find "w3m")
   (locate-library "w3m")))

;; Initialization

(when (w3m-is-present)
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
  (setq browse-url-browser-function 'w3m-browse-url)
  
  ;; Use UTF-8
  (setq w3m-coding-system 'utf-8
        w3m-file-coding-system 'utf-8
        w3m-file-name-coding-system 'utf-8
        w3m-input-coding-system 'utf-8
        w3m-output-coding-system 'utf-8
        w3m-terminal-coding-system 'utf-8)

  ;; Enable cookies
  (setq w3m-use-cookies t))

;;; WIKI EDITING
(defvar default-wiki
  '("metaverse" "http://metaverse.hceris.com/wiki" utf-8 "")
  "The key that defines my default wiki. Arguments are wiki name, url, encoding, captcha")

(defvar moinmoin-edit-suffix
  "action=edit&editor=text"
  "The suffix that should be appended to a page name to edit it")

(defun wiki-url (host pagename suffix)
  "Builds a wiki url, including an action (suffix), if it is non-nil"
  (format "%s/%s%s"
          host
          pagename
          (if suffix (format "?%s" suffix) "")))

(defun sanitize-pagename (pagename)
  (if (string-match "^\\." pagename)
      pagename
    (capitalize pagename)))

(defun wiki-view (pagename)
  (interactive "sPage to view: \n")
  (w3m-browse-url (wiki-url
                   (cadr default-wiki)
                   (sanitize-pagename pagename)
                   nil)))

(defun wiki-edit (pagename)
  (interactive "sPage to edit: \n")
  (w3m-browse-url (wiki-url
                   (cadr default-wiki)
                   (sanitize-pagename pagename)
                   moinmoin-edit-suffix))
                                        ; Doesn't work, code is run before page is loaded
  ;; (w3m-next-form)
  ;; (w3m-view-this-url)
  )

(defun wiki-help ()
  (wiki-view "SyntaxReference"))

(defun wiki-edit-newbook ()
  (interactive)
  (wiki-edit (format "Libros/%s"
                     (format-time-string "%Y" (current-time)))))

(provide 'w3m-config)
;;; w3m-config.el ends here

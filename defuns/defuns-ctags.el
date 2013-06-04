;;; defuns-ctags.el --- Functions for manipulating tags
;;
;; To use this, you need to install exuberant-ctags
;;
;;  - linux: sudo apt-get install exuberant-ctags
;;  - Mac OS X: brew install ctags

(defun build-ctags ()
  (interactive)
  (message "building project tags")
  (let ((root (eproject-root)))
    (shell-command (concat "ctags -e -R --extra=+fq --exclude=db --exclude=test --exclude=.git --exclude=public -f " root "TAGS " root)))
  (visit-project-tags)
  (message "tags built successfully"))

(defun visit-project-tags ()
  (interactive)
  (let ((tags-file (concat (eproject-root) "TAGS")))
    (visit-tags-table tags-file)
    (message (concat "Loaded " tags-file))))

(defun my-find-tag ()
  (interactive)
  (if (file-exists-p (concat (eproject-root) "TAGS"))
      (visit-project-tags)
    (build-ctags))
  (if (region-active-p)
      (etags-select-find (buffer-substring-no-properties (region-beginning) (region-end)))
    (etags-select-find-tag-at-point)))

(provide 'defuns-ctags)

;;; defuns-ctags.el --- Functions for manipulating tags
;;
;; To use this, you need to install exuberant-ctags
;;
;;  - linux: sudo apt-get install exuberant-ctags
;;  - Mac OS X: brew install ctags

;; TODO: remove when the package is not loaded from vendor anymore
;; TODO 2: Or better yet, transition to the find tag stuff in projectile
(require 'etags-select)

(defun build-ctags ()
  (interactive)
  (message "building project tags")
  (let ((root (projectile-project-root)))
    (shell-command (concat (executable-find "ctags") " -e -R --extra=+fq"
                           " --exclude=db --exclude=doc --exclude=log --exclude=tmp --exclude=.git --exclude=public"
                           " -f " root "TAGS " root)))
  (visit-project-tags)
  (message "tags built successfully"))

(defun visit-project-tags ()
  (interactive)
  (let ((tags-file (concat (projectile-project-root) "TAGS")))
    (visit-tags-table tags-file)
    (message (concat "Loaded " tags-file))))

(defun create-or-visit-project-tags ()
  (if (file-exists-p (concat (projectile-project-root) "TAGS"))
      (visit-project-tags)
    (build-ctags)))

(defun my-find-tag ()
  (interactive)
  (create-or-visit-project-tags)
  (if (region-active-p)
      (etags-select-find (buffer-substring-no-properties (region-beginning) (region-end)))
    (etags-select-find-tag-at-point)))

;; Special functions for ruby

(defun thing-after-point ()
  "Things after point, including current symbol."
  (if (thing-at-point 'symbol)
      (save-excursion
        (let ((from (beginning-of-thing 'symbol))
              (to   (end-of-thing 'line)))
          (and (> to from)
               (buffer-substring-no-properties from to))))))

(defun ruby-thing-at-point ()
  "Get ruby thing at point.
   1. thing at 'current_user'   get current_user;
   2. thing at '!current_user'  get current_user;
   3. thing at 'current_user!'  get current_user!;
   4. thing at 'current_user='  get current_user=;
   5. thing at 'current_user =' get current_user=;
   6. thing at 'current_user ==' get current_user;
   7. thing at 'current_user ||=' get current_user=;
   Otherwise, get `find-tag-default symbol."
  (let ((symbol (thing-at-point 'symbol))
        (remain (thing-after-point)))
    (if (and symbol remain)
        (let ((sym (s-chop-prefixes '("!!" "!") symbol))
              (rem (s-chop-prefixes '("!!" "!") remain)))
          (if (s-matches? (concat "^" sym "\\( *\\(||\\)?=[^=]\\)") rem)
              (concat sym "=")
            sym))
      (find-tag-default))))

(defun ruby-find-tag ()
  (interactive)
  (create-or-visit-project-tags)
  (if (region-active-p)
      (etags-select-find (buffer-substring-no-properties (region-beginning) (region-end)))
    (etags-select-find (ruby-thing-at-point))))

(provide 'defuns-ctags)

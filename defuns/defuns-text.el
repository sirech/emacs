;;; defuns-text.el --- Custom functions for text processing

;; Credit to http://www.emacswiki.org/emacs/CopyingWholeLines
(defun copy-line (&optional arg)
  "Do a kill-line but copy rather than kill.  This function directly calls
    kill-line, so see documentation of kill-line for how to use it including prefix
    argument and relevant variables.  This function works by temporarily making the
    buffer read-only."
  (interactive "P")
  (let ((buffer-read-only t)
        (kill-read-only-ok t))
    (kill-line arg)))

;; Surround
(defun surround-region-with (left-text right-text beg end)
  "Surrounds the marked region with the given left and right strings."
  (interactive "sLeft: \nsRight: \nr")
  (save-excursion
    (goto-char beg)
    (insert left-text)
    (goto-char (+ end (length left-text)))
    (insert right-text)))

(defun surround-region-with-htmltag (tag-name beg end)
  "Surrounds the marked region with the given html tag"
  (interactive "sTag name: \nr")
  (surround-region-with (concat "<" tag-name ">") (concat "</" tag-name ">") beg end))

(defun surround-rectangle-with (start end string-left string-right)
  "Surrounds the marked rectangle with the given left and right strings"
  (interactive
   (progn (barf-if-buffer-read-only)
          (list
           (region-beginning)
           (region-end)
           (read-string "Left: ")
           (read-string "Right: "))))
  ;; (read-string (format "Right (default %s): "
  ;;                    (or string-left ""))
  ;;            nil nil string-left nil))))
  (apply-on-rectangle '(lambda (startcol endcol string-left string-right)
                         (move-to-column startcol t)
                         (insert string-left)
                         (move-to-column (+ endcol (length string-left)) t)
                         (insert string-right))
                      start
                      end
                      string-left
                      string-right))

;; Uniquify
(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

;; Credit to
;; http://groups.google.com/group/gnu.emacs.help/msg/a784fbb684a24e17
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

;; Credit to http://emacs-fu.blogspot.com/2009/11/copying-lines-without-selecting-them.html
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
                                                                       "Copied line") (list (line-beginning-position) (line-beginning-position
                                                                                                                       2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; Count words
(defun wc (&optional start end)
  "Prints number of lines, words and characters in region or whole buffer."
  (interactive)
  (let ((n 0)
        (start (if mark-active (region-beginning) (point-min)))
        (end (if mark-active (region-end) (point-max))))
    (save-excursion
      (goto-char start)
      (while (< (point) end) (if (forward-word 1) (setq n (1+ n)))))
    (message "%3d %3d %3d" (count-lines start end) n (- end start))))

;; Temporarily display line number when jumping to a given line
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(provide 'defuns-text)

;;; mode-line-config.el --- The configuration for the mode line
(setq-default mode-line-format
      (list
       ;; the buffer name; the file name as a tool tip
       '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                           'help-echo (buffer-file-name)))

       ;; line and column
       "(" ;; '%02' to set to 2 chars at least; prevents flickering
       (propertize "%03l" 'face 'font-lock-comment-face) ","
       (propertize "%03c" 'face 'font-lock-comment-face)
       ") "

       ;; relative position, size of file
       "["
       (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
       "/"
       (propertize "%I" 'face 'font-lock-constant-face) ;; size
       "] "

       ;; the current major mode for the buffer.
       "["

       '(:eval (propertize mode-name 'face 'font-lock-string-face
                           'help-echo buffer-file-coding-system))

       minor-mode-alist

       "] "


       "[" ;; insert vs overwrite mode, input-method in a tooltip

       ;; was this buffer modified since the last save?
       '(:eval (when (buffer-modified-p)
                 (propertize "Mod"
                             'face 'font-lock-function-name-face
                             'help-echo "Buffer has been modified")
                 ))

       ;; is this buffer read-only?
       '(:eval (when buffer-read-only
                 (propertize "RO"
                             'face 'font-lock-type-face
                             'help-echo "Buffer is read-only")))
       "] "

       "%-" ;; fill with '-'
       ))

(provide 'mode-line-config)

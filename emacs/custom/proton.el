;;; proton --- Proton Mail productivity tricks

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defun pm-oa (&optional ARG)
  "Keyboard macro.  arg ARG."
  (interactive "p")
  (let ((pos (point)))
    (goto-char (point-min))
    (search-forward "use ")
    (beginning-of-line)
    (insert "use OpenApi\\Annotations as OA;\nuse Nelmio\\ApiDocBundle\\Annotation\\Model;\n")
    (goto-char pos)))

(defun pm-indent-comment-section (level start end)
  "Recursively indent annotations in a comment within a region."

  )

(defun nesting (level search)
  "Keep track of nesting level.

arg LEVEL  indentation level.
arg SEARCH  string to search."
  (let ((l level) (start 0) (regex "\\([[({]\\)\\|[])}]"))
    ;; Reset search
    (string-match "" "")
    (while (string-match regex search (match-end 0))
      (if (match-string 1 search) (cl-incf l) (cl-decf l)))
    l))

(defun pm-format-comment (&optional _)
  "Format comment block."
  (interactive "p")
  (save-excursion
    (let ((pos (point)) (level 0))
        (search-forward "*/")
        (push-mark (line-beginning-position) t t)
        (search-backward "/**" nil t)
        (beginning-of-line)
        (narrow-to-region (line-beginning-position) (mark))
        (indent-region (point) (mark))
        (while (< (line-beginning-position 2) (mark))
            (beginning-of-line 2)
            (condition-case nil
                (let ((eolp (line-end-position)))
                  (search-forward "*" eolp nil)
                  (re-search-forward "[[:space:]]*"  nil nil 1)
                  (replace-match " ")
                  (let ((tmplevel (nesting level
                                          (buffer-substring
                                           (point)
                                           (save-excursion
                                             (re-search-forward "[][:space:],})]*" (line-end-position)) (point))))))
                    (insert (make-string (* 4 tmplevel) ? ))))
              (error nil))
            (setq level (nesting level (buffer-substring (point) (line-end-position))))
            )
        (pop-mark)
        (widen))))

(defun pm-test-drive (&optional filter)
  "Run drive tests. arg filter."
  (interactive "p")
  )

(defun pm-test-quick (&optional filter)
  "Run quick suite tests. arg filter."
)

(provide 'proton)
;;; proton.el ends here

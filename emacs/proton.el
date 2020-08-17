;;; proton --- Proton Mail productivity tricks

;;; Commentary:

;;; Code:

(defun pm-oa (&optional ARG)
  "Keyboard macro.  arg ARG."
  (interactive "p")
  (let ((pos (point)))
    (goto-char (point-min))
    (search-forward "use ")
    (beginning-of-line)
    (insert "use OpenApi\\Annotations as OA;\nuse Nelmio\\ApiDocBundle\\Annotation\\Model;\n")
    (goto-char pos)))

(provide 'proton)
;;; proton.el ends here

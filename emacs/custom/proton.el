;;; proton --- Proton Mail productivity tricks

;;; Commentary:

;;; Code:

(require 'cl-lib)

(print "yes")

(setq iphlicence (let ((licf
			(expand-file-name "~/intelephense/LICENCE.txt")))
		   (if
		       (file-exists-p licf)
		       (with-temp-buffer
			 (insert-file-contents licf)
			 (string-trim
			  (buffer-string)))
		     "")))

(leaf company-phpactor :ensure t)

(leaf php-mode
  :ensure t yasnippet-snippets
  :require dap-php
  :custom
  (php-mode-coding-style . (quote symfony2))
  (lsp-intelephense-licence-key . iphlicence)
  (lsp-intelephense-php-version . "8.1.0")
  :hook
  (php-mode-hook . yas-minor-mode)
  (php-mode-hook . (lambda () (set (make-local-variable 'company-backends)
				   '(;; list of backends
				     company-capf
				     company-phpactor
				     ))))
  )

(leaf swift-mode
  :ensure t)

(leaf flycheck-phpstan
      :ensure t)

(leaf php-cs-fixer
      :ensure t)


(leaf mermaid-mode
      :ensure t
)

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
  "Run drive tests.
arg FILTER."
  (interactive "p")
  )

(defun pm-test-quick (&optional filter)
  "Run quick suite tests.
arg FILTER."
)

(defun pm-get-ns (file-name project-root)
  "Derive namespace from filename.
arg FILE-NAME current buffer's file name PROJECT-ROOT path to project root"
  (let* ((path (directory-file-name (file-relative-name (file-name-directory file-name) (concat project-root))))
    (prefix (if (string-match "^apps/[[:word:]]+/tests/" path) "Tests\\" (if (string-match "^/apps" path) "Proton\\Apps\\") "Proton\\Bundles\\"))
    (ns (concat prefix (replace-regexp-in-string "/" "\\" path t t))))
    (replace-regexp-in-string "\\\\\\(apps?\\|tests\\|src\\|bundles\\)\\\\" "\\\\" ns t))
  )

;; Work laptop use kde wallet
(if (string= (system-name) "work-anthill")
    (progn (setq auth-sources '("secrets:kdewallet"))))

;; setup forge
(with-eval-after-load 'forge
  (add-to-list 'forge-alist
               '("gitlab.protontech.ch" "gitlab.protontech.ch/api/v4" "gitlab.protontech.ch" forge-gitlab-repository)))

(defun my-open-phpstorm ()
  "Open file in phpstorm."
  (interactive)
  (shell-command (concat "nohup phpstorm " (shell-quote-argument (buffer-file-name)) " &") "*phpstorm*" "*phpstorm-errors*")
  )

(defun pm-id-decrypt (encrypted-id)
  (shell-command-to-string (concat "kubectl -n env-dev exec services/slim-api -c slim-api -- ./quark idcrypt -d " (shell-quote-argument encrypted-id))))
                                        ;(pm-id-decrypt "OQCSAHH0TrEx_kRy6QEM4hxXXTjMaG9GAFiBYUicLBuOHKXURZ1xx2C-AKzG-QrWnxCrZQ_AGwxH4bM_eemQyw==")

(defun pm-id-encrypt (internal-id)
  (shell-command-to-string (concat "kubectl -n env-dev exec services/slim-api -c slim-api -- ./quark idcrypt " (shell-quote-argument internal-id))))


(provide 'proton)
;;; proton.el ends here

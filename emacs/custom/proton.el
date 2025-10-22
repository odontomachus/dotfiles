;;; proton --- Proton Mail productivity tricks

;;; Commentary:

;;; Code:

(require 'cl-lib)

(print "Loading proton mode")

;; (add-hook 'csharp-mode-hook 'lsp-deferred)
;; (add-hook 'csharp-ts-mode-hook 'lsp-deferred)
;; (add-hook 'php-mode-hook 'lsp-deferred)


(setq iphlicence (let ((licf
			(expand-file-name "~/intelephense/LICENCE.txt")))
		   (if
		       (file-exists-p licf)
		       (with-temp-buffer
			 (insert-file-contents licf)
			 (string-trim
			  (buffer-string)))
		     "")))

(use-package company-phpactor :ensure t)

(use-package php-mode
  :ensure t
  :custom
  (flycheck-php-phpcs-executable "~/.config/composer/vendor/bin/phpcs")
  (flycheck-php-phpmd-executable "~/.config/composer/vendor/bin/phpmd")
  (phpcbf-executable "~/.config/composer/vendor/bin/phpcbf")
  (flycheck-phpcs-standard "PSR12")
  (php-mode-coding-style (quote symfony2))
  (lsp-intelephense-licence-key iphlicence)
  (lsp-intelephense-php-version "8.2.0")
  :hook
  (php-mode-hook . yas-minor-mode)
;;  (php-mode-hook . lsp-deferred)
  (php-mode-hook . (lambda () (set (make-local-variable 'company-backends)
				   '(;; list of backends
				     company-capf
				     company-phpactor
				     ))))
  )

;; (use-package dap-mode
;;   :ensure t
;; ;;  :after lsp-mode
;;   :config
;;   (dap-mode t)
;;   (dap-ui-mode t)
;;   (dap-register-debug-template "PHP"
;;                                (list :type "php"
;;                                      :cwd nil
;;                                      :request "launch"
;;                                      :name "Php Debug"
;;                                      :args '("--server=9000")
;;                                      :pathMappings (ht ("/var/www/api" (projectile-project-root (buffer-file-name))))
;;                                      :sourceMaps t))
;;   )

(use-package swift-mode
  :ensure t)

(use-package flycheck-phpstan
      :ensure t)

(use-package php-cs-fixer
      :ensure t)


(use-package mermaid-mode
      :ensure t
)

(use-package kotlin-mode
  :ensure t
  :init (add-to-list 'exec-path "~/.emacs.d/.cache/lsp/kotlin/server/bin/")
;;  :hook (kotlin-mode-hook . lsp-deferred)
  )

(use-package eglot
  :ensure t
  :hook
  (php-mode . eglot-ensure)
  (kotlin-mode . eglot-ensure)
  (csharp-ts-mode . eglot-ensure)
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

(setq pm-idcrypt-cmd (if (executable-find "pm-idcrypt") "pm-idcrypt " "kubectl --context atlas -n env-dev exec services/slim-api -c slim-api -- ./quark idcrypt "))
(defun pm-id-decrypt (encrypted-id)
  "Decrypt an id. (ENCRYPTED-ID id to decrypt)"
  (string-trim
   (shell-command-to-string (concat pm-idcrypt-cmd "-d -- " (shell-quote-argument encrypted-id)))))

(defun pm-id-encrypt (internal-id)
  (string-trim
   (shell-command-to-string (concat pm-idcrypt-cmd (shell-quote-argument internal-id)))))

;(pm-id-decrypt "OQCSAHH0TrEx_kRy6QEM4hxXXTjMaG9GAFiBYUicLBuOHKXURZ1xx2C-AKzG-QrWnxCrZQ_AGwxH4bM_eemQyw==")


(defun pm-id-decrypt-interactive (beginning end)
  (interactive "r")
  (let '(decrypted
         (let '(encrypted (buffer-substring beginning end))
           (pm-id-decrypt encrypted)))
    (progn
      (goto-char end)
      (insert " (" decrypted ")"))))

(defun pm-id-encrypt-interactive (beginning end)
  (interactive "r")
  (let '(encrypted
         (let '(decrypted (buffer-substring beginning end))
           (pm-id-encrypt decrypted)))
    (progn
      (goto-char end)
      (insert " (" encrypted ")"))))

(keymap-global-set "C-c j p e" 'pm-id-encrypt-interactive)
(keymap-global-set "C-c j p d" 'pm-id-decrypt-interactive)


(provide 'proton)
;;; proton.el ends here

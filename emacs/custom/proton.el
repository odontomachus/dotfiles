;;; proton --- Proton Mail productivity tricks

;;; Commentary:

;;; Code:

(require 'cl-lib)

(print "Loading proton mode")

(setq iphlicence (let ((licf
			(expand-file-name "~/intelephense/LICENCE.txt")))
		   (if
		       (file-exists-p licf)
		       (with-temp-buffer
			 (insert-file-contents licf)
			 (string-trim
			  (buffer-string)))
		     "")))

(setq claude-code-terminal-backend 'vterm)

(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  ;; :config
  ;; ;; optional IDE integration with Monet
  ;; (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  ;; (monet-mode 1)

  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

(use-package company-phpactor :ensure t)

(use-package php-mode
  :ensure t
  :custom
  (phpcbf-executable "~/.config/composer/vendor/bin/phpcbf")
  (php-mode-coding-style (quote symfony2))
  (lsp-intelephense-licence-key iphlicence)
  (lsp-intelephense-php-version "8.4.0")
  :hook
  (php-mode-hook . yas-minor-mode)
;;  (php-mode-hook . lsp-deferred)
  (php-mode-hook . (lambda () (set (make-local-variable 'company-backends)
				   '(;; list of backends
				     company-capf
				     company-phpactor
				     ))))
  )

(with-eval-after-load 'csharp-ts-mode
  (define-key csharp-ts-mode-map (kbd "C-c p c")
    (lambda ()
      (interactive)
      (let ((sln (my/find-csharp-solution (project-current t))))
        (compile (concat "dotnet build" (when sln (concat " " sln)))))))
  (define-key csharp-mode-map (kbd "C-c p t")
    (lambda ()
      (interactive)
      (let ((sln (my/find-csharp-solution (project-current t))))
        (compile (concat "dotnet test" (when sln (concat " " sln))))))))

(defun my/find-csharp-solution (project)
  "Find a .slnx file in PROJECT root or one level deep, preferring non-Public."
  (let* ((root (project-root project))
         (all-slnx (directory-files root t "\\.slnx$"))
         (candidates (or all-slnx
                         (let ((found '()))
                           (dolist (dir (directory-files root t "^[^.]"))
                             (when (file-directory-p dir)
                               (setq found (append found (directory-files dir t "\\.slnx$")))))
                           found)))
         (non-public (seq-remove (lambda (f) (string-match-p "Public" f)) candidates)))
    (car (or non-public candidates))))

(defun my/roslyn-lsp-command (_)
  "Build the command line for Roslyn."
  (list "~/.local/share/roslyn-lsp/content/LanguageServer/linux-x64/Microsoft.CodeAnalysis.LanguageServer"
        "--stdio" "--logLevel" "Information" "--extensionLogDirectory" "/tmp/roslyn-logs"))

(defun my/csharp-eglot-setup ()
  "Configure eglot workspace settings for C# with the correct solution file."
  (when-let* ((proj (project-current))
              (sln (my/find-csharp-solution proj)))
    (setq-local eglot-workspace-configuration
                `(:csharp (:solution ,sln)))))

(use-package swift-mode
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
  :config
  (add-to-list 'eglot-server-programs
               `((php-mode :language-id "php") . ("intelephense" "--stdio" :initializationOptions
                                                  (:licenseKey ,iphlicence))))
  ;; (add-to-list 'eglot-server-programs
  ;;              '((csharp-ts-mode :language-id "csharp") . my/roslyn-lsp-command))
  :hook
  (php-mode . eglot-ensure)
  (kotlin-mode . eglot-ensure)
  (csharp-ts-mode . my/csharp-eglot-setup)
  (csharp-ts-mode . eglot-ensure))

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
;; https://magit.vc/manual/forge.html#Setup-for-Another-Gitlab-Instance
(with-eval-after-load 'forge
  (add-to-list 'forge-alist
               '("gitlab.protontech.ch" "gitlab.protontech.ch/api/v4" "gitlab.protontech.ch" forge-gitlab-repository)))

;; https://www.gnu.org/software/emacs/manual/html_mono/auth.html#Top
;; (setq auth-sources '((:source (:secrets default)
;;                      :host "myserver" :user "joe")
;;                     "~/.authinfo.gpg"))

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

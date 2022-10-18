;;; Init --- Configuration for emacs.;

;;; Commentary:

;; By Jonathan Villemaire-Krajden
;; Inspired by lots of online sources.

;;; Code:

(savehist-mode 1)

;;(setq debug-on-quit t)
;; (setq lsp-print-io t)

(setq iphlicence (let ((licf
			(expand-file-name "~/intelephense/LICENCE.txt")))
		   (if
		       (file-exists-p licf)
		       (with-temp-buffer
			 (insert-file-contents licf)
			 (string-trim
			  (buffer-string)))
		     "")))

;; (require 'notifications)
(add-to-list 'load-path "~/.emacs.d/custom/")
(require 'proton)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      delete-old-versions 4
      version-control nil
      vc-make-backup-files nil
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
      savehist-file "~/.emacs.d/savehist"
      history-length 1000
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      savehist-additional-variables '(kill-ring
                                      search-ring
                                      regexp-search-ring)
      tooltip-use-echo-area t
      show-trailing-whitespace t
      confirm-nonexistent-file-or-buffer nil
      gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024 4)
      ;; Speedup long lines
      bidi-inhibit-bpa t
      global-visual-line-mode t
      )

(tool-bar-mode -1)
(menu-bar-mode -1)

;; Tooltips in echo area
(tooltip-mode -1)

(setq-default indent-tabs-mode nil)

(let* ((node_path (expand-file-name "~/.nvm/versions/node/"))
       (version (car (reverse (sort (directory-files node_path) 'string-collate-lessp))))
       )
  (if (string-match-p "^v[0-9]\\{2\\}\\." version)
      (add-to-list 'exec-path (concat node_path version "/bin"))
      ))

;; y/n prompt only, no yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Disable prompt on closing buffer with active process
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(require 'uniquify)

;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)
;; (setq ido-auto-merge-delay-time 1.5)
;; ;; No prompt when creating new buffer
;; (setq ido-create-new-buffer 'always)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'leaf)
  (progn
   (package-refresh-contents)
   (package-install 'leaf)))
(require 'leaf)

(leaf feather
  :ensure t
  :config (feather-mode))

(leaf company
      :ensure t
      :custom
      (company-minimum-prefix-length . 1)
      (company-tooltip-align-annotations . t)
      (company-idle-delay . 0.3)
      :init
      (global-set-key  (kbd "C-c <tab>") 'company-complete-common)
      (global-company-mode))

(leaf solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t))

(leaf rainbow-delimiters
  :ensure t)

(leaf async
      :leaf-defer nil
      :custom (async-bytecomp-package-mode . t))

(leaf kotlin-mode
  :ensure t
  :init (add-to-list 'exec-path "~/.emacs.d/.cache/lsp/kotlin/server/bin/")
  :hook (kotlin-mode-hook . lsp-deferred)
  )

(leaf go-mode
  :ensure t
  :hook (go-mode-hook . lsp-deferred)
  )

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(async-bytecomp-package-mode t)
 '(company-idle-delay 0.3)
 '(company-minimum-prefix-length 1)
 '(custom-safe-themes
   '("00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
 '(delq nil t)
 '(eldoc-idle-delay 0.3)
 '(flycheck-phpcs-standard "PSR12")
 '(global-auto-revert-mode t)
 '(graphviz-dot-indent-width 4)
 '(package-selected-packages
   '(highlight-indentation typescript-mode yaml-mode php-mode phpactor pyvenv dap-mode lsp-mode magit flycheck ace-window projectile company leaf yasnippet-snippets which-key web-mode tide solarized-theme rustic rainbow-delimiters plantuml-mode php-cs-fixer ox-reveal lsp-ui lsp-java lice kotlin-mode jedi helm-projectile helm-ag graphviz-dot-mode go-mode gitlab-ci-mode git-link forge flycheck-phpstan feather elpy elixir-mode company-phpactor company-jedi))
 '(plantuml-default-exec-mode 'executable t)
 '(plantuml-executable-path "/usr/bin/plantuml" t)
 '(plantuml-jar-path "/usr/share/java/plantuml.jar" t)
 '(safe-local-variable-values
   '((php-project-root . git)
     (php-project-root . /home/jonathan/projects/proton/containers/webserver/repos/api)
     (php-project-root . default-directory)))
 '(split-height-threshold 160)
 '(warning-suppress-types '((emacs))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'org)
(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (java . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (gnuplot . t)
         (clojure . t)
         (org . t)
         (plantuml . t)
         (latex . t))))

(leaf which-key
      :ensure t
      :init (which-key-mode))

(leaf projectile
      :ensure t
      :init (setq projectile-keymap-prefix (kbd "C-c p"))
      :config
      (projectile-mode +1)
      (projectile-register-project-type 'php '("composer.json")
					:src-dir "apps"
					:test "composer test"
					:run "composer serve"
					:test-suffix "Test"
					:test-dir "tests")
      (projectile-register-project-type 'js '("package.json")
					:test "composer test"
					:run "composer serve"
					:test-suffix "Test"
					:test-dir "tests"))

(leaf ace-window
      :ensure t)
(global-set-key (kbd "C-c o") 'ace-window)

(leaf helm-projectile
      :after projectile
      :ensure t
      :init
      (helm-projectile-on)
      )

(leaf helm-ag
      :ensure t
      )

(defun gen-password (&optional len)
  "Generate a random password.
Use `C-u` <N> to specify length.
LEN length of password to generate in bytes.  Default is 16
Depends on system gpg."
  (interactive "P")
  (or len (setq len 16))
  (insert (seq-take
           (shell-command-to-string (format "gpg --gen-random --armor 1 %d" len))
           len)))

(global-set-key
 (kbd "C-c n p")
 'gen-password
 )

(global-set-key
 (kbd "C-c n d")
 'org-time-stamp
 )

(defun current-date (&optional arg)
  "Insert current date.
ARG nil
Insert current date at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d"))
  )

(global-set-key
 (kbd "C-c n D")
 'current-date
 )

(defun my-test-emacs ()
  "Test my Emacs config."
  (interactive)
  (require 'async)
  (async-start
   (lambda () (shell-command-to-string
               "emacs --batch --eval \"
(condition-case e
    (progn
      (load \\\"~/.emacs.d/init.el\\\")
      (message \\\"-OK-\\\"))
  (error
   (message \\\"ERROR!\\\")
   (signal (car e) (cdr e))))\""))
   `(lambda (output)
      (if (string-match "-OK-" output)
          (when ,(called-interactively-p 'any)
            (message "All is well"))
        (switch-to-buffer-other-window "*startup error*")
        (delete-region (point-min) (point-max))
        (insert output)
        (search-backward "ERROR!")))))

;; Licence headers & content
(leaf lice
      :ensure t)

(leaf flycheck
      :ensure t
      :custom
      (flycheck-php-phpcs-executable . "~/.config/composer/vendor/bin/phpcs")
      (flycheck-php-phpmd-executable . "~/.config/composer/vendor/bin/phpmd")
      (phpcbf-executable . "~/.config/composer/vendor/bin/phpcbf")
      (flycheck-phpcs-standard . "PSR12")
      :config (global-flycheck-mode t))

(leaf magit
      :ensure t)

(leaf forge
  :ensure t magit
  :after magit)

(leaf gitlab-ci-mode
      :ensure t magit)

(leaf lsp-mode
      :ensure t company
      :init (setq lsp-keymap-prefix (kbd "C-c l"))
      :custom (lsp-prefer-capf . t)
      (lsp-eldoc-enable-hover . t)
      (lsp-log-io . nil)
      (lsp-semantic-highlighting . t)
      (lsp-enable-xref . t)
      (lsp-signature-auto-activate . t)
      (lsp-signature-render-documentation . t)
      (lsp-file-watch-ignored . '("[/\\\\]\\.git$" "[/\\\\]\\.hg$" "[/\\\\]\\.bzr$" "[/\\\\]_darcs$" "[/\\\\]\\.svn$" "[/\\\\]_FOSSIL_$" "[/\\\\]\\.idea$" "[/\\\\]\\.ensime_cache$" "[/\\\\]\\.eunit$" "[/\\\\]node_modules$" "[/\\\\]\\.fslckout$" "[/\\\\]\\.tox$" "[/\\\\]\\.stack-work$" "[/\\\\]\\.bloop$" "[/\\\\]\\.metals$" "[/\\\\]target$" "[/\\\\]\\.ccls-cache$" "[/\\\\]\\.deps$" "[/\\\\]build-aux$" "[/\\\\]autom4te.cache$" "[/\\\\]\\.reference$" "[/\\\\]vendor" "[/\\\\]api-spec" "[/\\\\]var" "[/\\\\]cache"))
      (lsp-file-watch-threshold . 30000)
      (lsp-intelephense-files-exclude .
                                      ["**/.git/**" "**/.svn/**" "**/.hg/**" "**/CVS/**" "**/.DS_Store/**" "**/node_modules/**" "**/bower_components/**" "**/vendor/**/{Test,test,Tests,tests}/**" "**/vendor/protonlabs/**"])
                                        ; (lsp-idle-display . 0.500)
      :hook (php-mode-hook . lsp-deferred)
      :commands (lsp))

(leaf lsp-ui
      :ensure t
      :after lsp-mode flycheck
      :custom
      (lsp-ui-sideline-enable . t)
      (lsp-ui-sideline-show-diagnostics . t)
      (lsp-ui-sideline-show-code-actions . t)
      (lsp-ui-sideline-show-hover . nil)
      (lsp-ui-peek-enable . t)
      (lsp-ui-peek--offset . 10)
      (lsp-ui-peek-always-show . t)
      (lsp-ui-peek-list-width . 92)
      (lsp-ui-peek-peek-height . 24)
      (lsp-ui-doc-enable . t)
      (lsp-ui-doc-show-with-cursor . t)
      (lsp-ui-doc-include-signature . t)
      (lsp-signature-auto-activate . t)
      (lsp-lens-enable . t)
      (lsp-signature-render-documentation . t)
      :hook (lsp-mode-hook . lsp-ui-mode))

(leaf dap-mode
      :ensure t :after lsp-mode
      :config
      (dap-mode t)
      (dap-ui-mode t)
      ;; (dap-register-debug-template "PHP"
      ;;                              (list :type "php"
      ;;                                    :cwd nil
      ;;                                    :request "launch"
      ;;                                    :name "Php Debug"
      ;;                                    :args '("--server=9000")
      ;;                                    :pathMappings (ht ("/var/www/api" (projectile-project-root (buffer-file-name))))
      ;;                                    :sourceMaps t))
      )

(leaf elixir-mode
      :ensure t)

(leaf rustic
      :ensure t
      )

(leaf elpy
      :ensure t jedi pyvenv company-jedi
      :after company
      :init (elpy-enable)
      :config
      (add-to-list 'company-backends 'elpy-company-backend)
      :custom (elpy-modules . (delq 'elpy-module-flymake elpy-modules))
      (elpy-rpc-backend . "jedi")
      (elpy-shell-echo-input . nil)
      :hook (elpy-mode-hook . flycheck-mode))


(leaf phpactor :ensure t)

(leaf company-phpactor :ensure t)

(leaf php-mode
      :ensure t yasnippet-snippets
      :custom
      (php-mode-coding-style . (quote symfony2))
      (lsp-intelephense-licence-key . iphlicence)
      :hook
      (php-mode-hook . yas-minor-mode)
      (php-mode-hook . (lambda () (set (make-local-variable 'company-backends)
				       '(;; list of backends
					 company-capf
					 company-phpactor
					 )))))

(leaf flycheck-phpstan
      :ensure t)

(leaf php-cs-fixer
      :ensure t)

(leaf yaml-mode
      :ensure t)

(leaf plantuml-mode
      :ensure t
      :custom (plantuml-executable-path . "/usr/bin/plantuml")
      (plantuml-default-exec-mode . 'executable)
      :config (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
      (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

(leaf web-mode
      :ensure t)

;; (setq help-at-pt-display-when-idle t)
;; (help-at-pt-set-timer)

(leaf graphviz-dot-mode
      :ensure t)

(leaf lsp-java :ensure t :after lsp
      :config (add-hook 'java-mode-hook 'lsp))

;; (leaf
;;   org-re-reveal
;;   :ensure nil
;;   :after org
;;   :custom (org-re-reveal-root . "https://cdn.jsdelivr.net/npm/reveal.js")
;;   :hook (org-mode . (require 'org-re-reveal))
;;  )

(leaf ox-reveal
      :ensure t
      :after org
      :hook (org-mode . (require 'ox-reveal)))

(leaf git-link
      :ensure t
      )

(leaf
 yasnippet-snippets
 :ensure t)

(leaf typescript-mode
      :ensure t
      :mode ("\\.ts$" "\\.tsx$")
      :hook (typescript-mode-hook . lsp))

(leaf tide
      :ensure t typescript-mode
      :hook ((typescript-mode . setup-tide)
             (javascript-mode . setup-tide)))

(defun my-open-phpstorm ()
  "Open file in phpstorm."
  (interactive)
  (shell-command (concat "nohup phpstorm &" (shell-quote-argument (buffer-file-name))))
  )

(provide 'init)
;;; init.el ends here

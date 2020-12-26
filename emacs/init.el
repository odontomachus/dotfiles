;;; Init --- Configuration for emacs.;

;;; Commentary:

;; By Jonathan Villemaire-Krajden
;; Inspired by lots of online sources.

;;; Code:

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      inhibit-startup-screen +1
      initial-scratch-message nil
      column-number-mode t)

;; Full frame
(if (<= (display-pixel-width) 1920) (set-frame-parameter nil 'fullscreen 'maximized)
  (progn
   (set-frame-parameter (selected-frame) 'fullscreen 'fullheight)
   (set-frame-width (selected-frame) (/ (display-pixel-width) 2) nil t)
   (set-frame-position (selected-frame) 0 0)
   (setq window-min-height (- (/ (window-body-height) 3) 1)
         window-min-width (- (/ (window-body-width) 3) 1))))

(savehist-mode 1)

(setq iphlicence (let ((licf
       (expand-file-name "~/intelephense/licence.txt")))
   (if
       (file-exists-p licf)
       (with-temp-buffer
         (insert-file-contents licf)
         (string-trim
          (buffer-string)))
     "")))

(require 'notifications)
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
      show-trailing-whitespace 't
      confirm-nonexistent-file-or-buffer nil
      gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024 4)
)

(tool-bar-mode -1)
(menu-bar-mode -1)

(add-to-list 'exec-path "~/.nvm/versions/node/v14.5.0/bin/")

;; Tooltips in echo area
(tooltip-mode -1)

(setq-default indent-tabs-mode nil)

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
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf))
(require 'leaf)

(leaf company
  :ensure t
  :custom
  (company-minimum-prefix-length . 1)
  (company-idle-delay . 0.3)
  :init
    (global-set-key  (kbd "C-c <tab>") 'company-complete-common)
    (global-company-mode))

(leaf solarized-theme
  :ensure t
)
(load-theme 'solarized-dark t)

(leaf async
  :leaf-defer nil
  :custom (async-bytecomp-package-mode . t))


(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(async-bytecomp-package-mode t)
 '(company-idle-delay 0.3)
 '(company-minimum-prefix-length 1)
 '(custom-safe-themes
   '("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
 '(delq nil t)
 '(elpy-modules nil)
 '(elpy-rpc-backend "jedi" t)
 '(elpy-shell-echo-input nil)
 '(flycheck-php-phpcs-executable "~/.config/composer/vendor/bin/phpcs")
 '(flycheck-php-phpmd-executable "~/.config/composer/vendor/bin/phpmd")
 '(flycheck-phpcs-standard "PSR12")
 '(global-auto-revert-mode t)
 '(lsp-eldoc-enable-hover t t)
 '(lsp-enable-xref t t)
 '(lsp-file-watch-ignored
   '("[/\\\\]\\.git$" "[/\\\\]\\.hg$" "[/\\\\]\\.bzr$" "[/\\\\]_darcs$" "[/\\\\]\\.svn$" "[/\\\\]_FOSSIL_$" "[/\\\\]\\.idea$" "[/\\\\]\\.ensime_cache$" "[/\\\\]\\.eunit$" "[/\\\\]node_modules$" "[/\\\\]\\.fslckout$" "[/\\\\]\\.tox$" "[/\\\\]\\.stack-work$" "[/\\\\]\\.bloop$" "[/\\\\]\\.metals$" "[/\\\\]target$" "[/\\\\]\\.ccls-cache$" "[/\\\\]\\.deps$" "[/\\\\]build-aux$" "[/\\\\]autom4te.cache$" "[/\\\\]\\.reference$" "[/\\\\]vendor" "[/\\\\]api-spec" "[/\\\\]var" "[/\\\\]cache") t)
 '(lsp-file-watch-threshold 30000 t)
 '(lsp-intelephense-files-exclude
   ["**/.git/**" "**/.svn/**" "**/.hg/**" "**/CVS/**" "**/.DS_Store/**" "**/node_modules/**" "**/bower_components/**" "**/vendor/**/{Test,test,Tests,tests}/**" "**/vendor/protonlabs/**"] t)
 '(lsp-log-io nil t)
 '(lsp-prefer-capf t t)
 '(lsp-response-timeout 25)
 '(lsp-semantic-highlighting t t)
 '(lsp-signature-auto-activate t t)
 '(lsp-signature-render-documentation t t)
 '(org-capture-templates
   '(("s" "Code snippets" entry
      (file "~/snippets.org")
      "")
     ("n" "Notes" entry
      (file "~/notes.org")
      "")))
 '(org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js" t)
 '(package-selected-packages
   '(company-jedi yasnippet-snippets yaml-mode web-mode tide solarized-theme rustic plantuml-mode phpcbf php-cs-fixer org-re-reveal magit lsp-ui lsp-java lice leaf jedi helm-projectile graphviz-dot-mode git-link flycheck-phpstan elpy elixir-mode company-phpactor))
 '(php-mode-coding-style 'symfony2 t)
 '(phpcbf-executable "~/.config/composer/vendor/bin/phpcbf" t)
 '(plantuml-default-exec-mode 'executable t)
 '(plantuml-executable-path "/usr/bin/plantuml" t)
 '(split-height-threshold 160))
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
         (ledger . t)
         (org . t)
         (plantuml . t)
         (latex . t))))

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
 (format-time-string "%Y-%m-%d")
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
  (lsp-ui-peek-always-show . t)
  (lsp-ui-peek-list-width . 92)
  (lsp-ui-peek-peek-height . 20)
  (lsp-ui-doc-include-signature . t)
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

(leaf phpcbf
  :ensure t
  :hook (php-mode . phpcbf-enable-on-save))

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

;; (leaf dap-java
;;   :ensure t
;;   :after lsp-java)

(leaf
  org-re-reveal
  :ensure t
  :after org
  :custom (org-re-reveal-root . "https://cdn.jsdelivr.net/npm/reveal.js")
  :hook (org-mode . (require 'org-re-reveal))
 )

(leaf git-link
  :ensure t
)

(leaf
  yasnippet-snippets
  :ensure t)

(defun setup-tide-mode ()
  "Setup tide mode."
  (interactive)
  (defun tide-imenu-index () nil)
  (tide-setup)
  (tide-hl-identifier-mode +1))


(leaf tide
  :ensure t typescript-mode company flycheck
  :after (typescript-mode company flycheck)
  :config ((add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
           (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode)))
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(defun my-open-phpstorm ()
  "Open file in phpstorm."
  (interactive)
  (shell-command (concat "nohup phpstorm " (shell-quote-argument (buffer-file-name))))
)

(provide 'init)
;;; init.el ends here

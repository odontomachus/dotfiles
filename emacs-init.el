;; No splash
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Start empty org mode screen
(setq inhibit-startup-screen +1)
;; (setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)

;; Full frame
(toggle-frame-maximized)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions 4)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))
(setq vc-follow-symlinks t)

(tool-bar-mode -1)
(menu-bar-mode -1)

;; Tooltips in echo area
;; (tooltip-mode -1)
;; (setq tooltip-use-echo-area t)

(setq-default indent-tabs-mode nil)
(setq show-trailing-whitespace 't)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; y/n prompt only, no yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Create file/buffer without prompt
(setq confirm-nonexistent-file-or-buffer nil)

;; Disable prompt on closing buffer with active process
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

(require 'uniquify)

(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(load-theme 'solarized-light t)

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
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(elpy-rpc-python-command "python3")
 '(jdee-server-dir "/home/villemai/lib/")
 '(js-indent-level 2)
 '(json-reformat:indent-width 2)
 '(org-plantuml-jar-path "/usr/share/java/plantuml.jar")
 '(package-selected-packages
   (quote
    (elpy lice hydra web-mode xref-js2 js2-refactor dap-mode lsp-java company-lsp ansible company-ansible flycheck-gradle flymake-gradle gradle-mode rope-read-mode jedi lsp-ui magit use-package tide plantuml-mode json-mode memory-usage mvn eclim company-emacs-eclim go-dlv django-mode docker-compose-mode dockerfile-mode ox-reveal git-link puppet-mode angular-mode ein jinja2-mode markdown-mode nginx-mode icicles helm-projectile helm groovy-mode dot-mode dumb-jump go-projectile go-mode terraform-mode solarized-theme babel yaml-mode oauth slack mmm-mode alchemist elixir-mode)))
 '(plantuml-jar-path "/usr/share/java/plantuml.jar")
 '(pyvenv-activate "/home/villemai/virtualenv/data/")
 '(typescript-indent-level 2))
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


(require 'icicles)
(icy-mode 1)

(package-initialize)
(elpy-enable)

(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


;; https://bbpcode.epfl.ch/browse/code/platform/collaboratory-extension-core/tree/README.md?h=refs/heads/master#n8
;; ssh://bbpcode.epfl.ch/platform/collaboratory-extension-core
(defun git-link-bbpcode (hostname dirname filename branch commit start end)
  (format "https://%s/browse/code/%s/tree/%s?%s#%s"
	  hostname
	  dirname
          filename
	  (if branch (format "h=%s" branch)
            (format "id=%s" commit))
          (format "n%s" start)))

(eval-after-load "git-link"
  '(progn
     (add-to-list 'git-link-remote-alist
                  '("bbpcode" git-link-bbpcode))))
(put 'upcase-region 'disabled nil)

(require 'company)
(global-company-mode t)

(require 'cc-mode)

(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

(use-package yasnippet :ensure t)
(use-package lsp-mode :ensure t)
(use-package hydra :ensure t)

(use-package company-lsp
  :after  company
  :ensure t
  :config
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t))

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point))

(use-package lsp-java :ensure t :after lsp
  :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :ensure t :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package dap-java :after (lsp-java))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)



(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.5)
(help-at-pt-set-timer)

(defun gen-password (&optional len)
  "Generate a random password. Requires gpg. Use C-u <N> to specify length. Default is 16."
  (or len (setq len 16))
  (interactive "P")
  (insert (seq-take
           (shell-command-to-string (format "gpg --gen-random --armor 1 %d" len))
           len)))

(global-set-key
 (kbd "C-c n p")
 'gen-password
 )

(require 'projectile)
(projectile-global-mode 1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(defun my-test-emacs ()
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

(add-to-list 'load-path "~/.emacs.d/local")
(require 'xwiki)

(dap-register-debug-provider
 "tomcat"
 (lambda (conf)
   (plist-put conf :debugPort 1234)
   (plist-put conf :host "localhost")
   conf))

(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))

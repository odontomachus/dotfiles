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

;; Python ide
(defun ome-elpy-setup ()
  (elpy-enable t)
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  (elpy-use-ipython)
  (setq elpy-rpc-backend "jedi")
  (when (executable-find "ipython")
    (elpy-use-ipython))
  (when (el-get-package-installed-p 'flycheck)
    (setq elpy-default-minor-modes
          (remove 'flymake-mode
                  elpy-default-minor-modes)))
  (define-key python-mode-map (kbd "RET")
    'newline-and-indent))

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
 '(jdee-server-dir "/home/villemai/lib/")
 '(js-indent-level 2)
 '(json-reformat:indent-width 2)
 '(package-selected-packages
   (quote
    (magit treemacs lsp-java use-package hydra dap-mode company-lsp tide plantuml-mode eglot json-mode memory-usage mvn eclim company-emacs-eclim go-dlv django-mode docker-compose-mode dockerfile-mode ox-reveal git-link ttl-mode n3-mode puppet-mode angular-mode ein jinja2-mode markdown-mode nginx-mode icicles helm-projectile helm groovy-mode dot-mode projectile-rails dumb-jump go-projectile go-mode terraform-mode solarized-theme babel yaml-mode oauth slack rvm mmm-mode alchemist elixir-mode)))
 '(plantuml-jar-path "/usr/share/java/plantuml.jar"))
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
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (latex . t))))

(require 'icicles)
;;(icy-mode 1)
(require 'projectile)
(projectile-global-mode 1)

(package-initialize)
(elpy-enable)
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt")

(autoload 'n3-mode "n3-mode" "Major mode for OWL or N3 files" t)

;; Turn on font lock when in n3 mode
(add-hook 'n3-mode-hook
          'turn-on-font-lock)

(setq auto-mode-alist
      (append
       (list
        '("\\.n3" . n3-mode)
        '("\\.owl" . n3-mode))
       auto-mode-alist))

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

;;(add-hook 'java-mode-hook 'my/java-hook)
(add-hook 'java-mode-hook 'eglot-ensure)

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
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

;; (defconst my/eclipse-jdt-home (expand-file-name "~/projects/vendor/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository"))

;; (defun my/eclipse-jdt-contact (interactive)
;;   (let ((cp (getenv "CLASSPATH")))
;;     (setenv "CLASSPATH" (concat cp ":" my/eclipse-jdt-home))
;;     (unwind-protect
;;         (eglot--eclipse-jdt-contact nil)
;;       (setenv "CLASSPATH" cp))))

;; (defun my/java-hook ()
;;   (progn
;;     (setcdr (assq 'java-mode eglot-server-programs) #'my/eclipse-jdt-contact)
;;     (eglot-ensure)
;; ))

;; (defconst my/eclipse-jdt-home "/tmp/jdt-language-server-latest.tar/plugins/org.eclipse.equinox.launcher_1.5.200.v20180922-1751.jar")

;; (defun my/eclipse-jdt-contact (interactive)
;;   (let ((cp (getenv "CLASSPATH")))
;;     (setenv "CLASSPATH" (concat cp ":" my/eclipse-jdt-home))
;;     (unwind-protect
;;         (eglot--eclipse-jdt-contact nil)
;;       (setenv "CLASSPATH" cp))))

;; (setcdr (assq 'java-mode eglot-server-programs) #'my/eclipse-jdt-contact)

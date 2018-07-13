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


(tool-bar-mode -1)
(menu-bar-mode -1)

;; Tooltips in echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

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

;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)
;; (setq ido-auto-merge-delay-time 1.5)
;; ;; No prompt when creating new buffer
;; (setq ido-create-new-buffer 'always)


(setq ruby-deep-indent-paren nil)
;; Our coding standard
(setq ruby-indent-level 2)

(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; (package-refresh-contents)

(defvar myPackages
  '(
    babel
    elpy
    ein
    elixir-mode
    alchemist
    flycheck
    mmm-mode
    py-autopep8
    icicles
    solarized-theme
    )
  )

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

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

(global-company-mode t)

(require 'py-autopep8)

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
 '(eclim-executable
   "/home/villemai/.eclipse/org.eclipse.platform_155965261_linux_gtk_x86_64/eclim")
 '(jdee-server-dir "/home/villemai/lib/")
 '(package-selected-packages
   (quote
    (memory-usage mvn eclim company-emacs-eclim go-dlv django-mode docker-compose-mode dockerfile-mode ox-reveal git-link ttl-mode n3-mode puppet-mode ac-html-angular angular-mode ein jinja2-mode markdown-mode nginx-mode icicles helm-projectile helm groovy-mode dot-mode projectile-rails dumb-jump go-projectile go-mode terraform-mode solarized-theme babel yaml-mode oauth slack rvm mmm-mode alchemist elixir-mode))))
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

;; regular auto-complete initialization
;; (require 'auto-complete-config)
;; (ac-config-default)

;; add the emacs-eclim source
;;(require 'ac-emacs-eclim-source)
;;(ac-emacs-eclim-config)



(require 'eclim)
(setq eclimd-autostart t)

(defun my-java-mode-hook ()
    (eclim-mode t))

(add-hook 'java-mode-hook 'my-java-mode-hook)

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)

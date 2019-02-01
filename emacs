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
    elixir-mode
    alchemist
    flycheck
    mmm-mode
    py-autopep8
    robe
    inf-ruby
    yari
    ruby-tools
    solarized-theme
    )
  )

(load-theme 'solarized-light t)

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

(require 'ruby-tools)
(add-hook 'ruby-mode-hook 'robe-mode)

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

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
 '(package-selected-packages
   (quote
    (plantuml-mode company-ansible company-erlang company-go company-jedi company-quickhelp company-racer company-shell company-web elixir-mix elixir-yasnippets elm-mode erlang erlstack-mode flycheck-elixir flycheck-mix flymake-elixir racer lsp-rust flymake-rust eglot lsp-mode tidy toml-mode flycheck-rust cargo rust-mode ess jinja2-mode markdown-mode nginx-mode icicles helm-projectile helm groovy-mode dot-mode rinari projectile-rails dumb-jump go-projectile go-mode terraform-mode solarized-theme babel yaml-mode oauth slack rvm mmm-mode alchemist elixir-mode))))
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
(add-hook 'rust-mode-hook 'cargo-minor-mode)

(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
(setq racer-rust-src-path "/home/jonathan/projects/vendor/rust/src/") ;; Rust source code PATH

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

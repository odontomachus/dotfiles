;; No splash
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Start empty org mode screen
(setq inhibit-startup-screen +1)
;; (setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)

;; Full frame
(set-frame-parameter nil 'fullscreen 'maximized)

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
      confirm-nonexistent-file-or-buffer nil)

(savehist-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)

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

(load-theme 'solarized-dark t)

(require 'leaf)

(leaf elpy
  :init (elpy-enable)
  :config (setq elpy-rpc-backend "jedi" elpy-shell-echo-input nil)

  (add-to-list 'company-backends 'elpy-company-backend)
  (require 'electric)
  (when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
  )


(leaf company
  :ensure t
)

(leaf async
  :leaf-defer nil
  :config (setq async-bytecomp-package-mode t))

(leaf rustic
  :ensure t
)



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
    (graphviz-dot-mode leaf lice magit flycheck-clang-analyzer company-ctags ctags elpy jedi pyvenv plantuml-mode company-ansible company-go company-quickhelp elixir-mix flycheck-elixir flycheck-mix lsp-rust lsp-mode ess jinja2-mode markdown-mode nginx-mode helm-projectile helm groovy-mode dot-mode dumb-jump go-projectile go-mode solarized-theme babel yaml-mode elixir-mode web-mode))))
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
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  :config
  (projectile-mode +1))

(add-hook 'python-mode-hook #'company-mode)

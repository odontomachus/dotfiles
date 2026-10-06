;;; Init --- Configuration for emacs.;

;;; Commentary:

;; By Jonathan Villemaire-Krajden
;; Inspired by lots of online sources.

;;; Code:

;;(setq debug-on-quit t)
;; (setq lsp-print-io t)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      delete-old-versions 4
      version-control nil
      vc-make-backup-files nil
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
      history-length 1000
      history-delete-duplicates t
      show-trailing-whitespace t
      confirm-nonexistent-file-or-buffer nil
      gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)
      ;; Speedup long lines
      bidi-inhibit-bpa t
      recentf-max-saved-items 100
      recentf-max-menu-items 10
      recentf-exclude '("~/\\..*" "^/tmp/")
      )

(savehist-mode 1)
(recentf-mode 1)
(global-visual-line-mode 1)
;; Tooltips in echo area
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)


(global-set-key (kbd "C-c C-w") 'subword-mode)
(global-set-key (kbd "C-c f") 'recentf)
(global-set-key (kbd "C-c j f") #'(lambda () (interactive) (kill-new buffer-file-name)))

(setq-default indent-tabs-mode nil)

(let* ((node_path (expand-file-name "~/.nvm/versions/node/"))
       (versions (and (file-directory-p node_path)
                      (directory-files node_path nil "\\`v[0-9]")))
       (latest (car (sort versions (lambda (a b)
                                      (version< (substring b 1) (substring a 1)))))))
  (when latest
    (let ((bin-dir (concat node_path latest "/bin")))
      (add-to-list 'exec-path bin-dir)
      ;; Also export PATH so subprocesses spawned via a real shell
      ;; (compile, async-shell-command, vterm) can find node/npm too -
      ;; exec-path alone only helps Emacs's own subprocess resolution.
      (setenv "PATH" (concat bin-dir path-separator (getenv "PATH"))))))

;; y/n prompt only, no yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Disable prompt on closing buffer with active process
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(require 'uniquify)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-check-signature 'allow-unsigned)
(package-initialize)

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :init
  (yas-global-mode t))

(use-package company
  :ensure t
  :custom
  (company-minimum-prefix-length 3)
  (company-tooltip-align-annotations t)
  (company-idle-delay 0.4)
  (company-dabbrev-downcase nil)
  (company-global-modes '(not minibuffer))
  :init
  (global-set-key (kbd "C-c <tab>") 'company-complete-common)
  (global-company-mode)
  :hook (org-mode-hook . (lambda ()
                           (setq-local company-idle-delay 0.5
                                       company-minimum-prefix-length 3)))
  )

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t))

(use-package rainbow-delimiters
  :ensure t)

(use-package ag
  :ensure t
  )

;; python poetry
(use-package poetry
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(async-bytecomp-package-mode t)
 '(custom-safe-themes
   '("00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c"
     "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3"
     "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879"
     default))
 '(delq nil t)
 '(eldoc-idle-delay 0.3)
 '(global-auto-revert-mode t)
 '(graphviz-dot-indent-width 2)
 '(lsp-file-watch-ignored-directories
   '("[/\\\\]\\.git$" "[/\\\\]\\.hg$" "[/\\\\]\\.bzr$" "[/\\\\]_darcs$"
     "[/\\\\]\\.svn$" "[/\\\\]_FOSSIL_$" "[/\\\\]\\.idea$"
     "[/\\\\]\\.ensime_cache$" "[/\\\\]\\.eunit$"
     "[/\\\\]node_modules$" "[/\\\\]\\.fslckout$" "[/\\\\]\\.tox$"
     "[/\\\\]\\.stack-work$" "[/\\\\]\\.bloop$" "[/\\\\]\\.metals$"
     "[/\\\\]target$" "[/\\\\]\\.ccls-cache$" "[/\\\\]\\.deps$"
     "[/\\\\]build-aux$" "[/\\\\]autom4te.cache$"
     "[/\\\\]\\.reference$" "[/\\\\]vendor" "[/\\\\]api-spec"
     "[/\\\\]var" "[/\\\\]cache") nil nil "Customized with use-package lsp-mode")
 '(markdown-code-lang-modes
   '(("ocaml" . tuareg-mode) ("elisp" . emacs-lisp-mode)
     ("ditaa" . artist-mode) ("asymptote" . asy-mode)
     ("dot" . fundamental-mode) ("sqlite" . sql-mode)
     ("calc" . fundamental-mode) ("C" . c-mode) ("cpp" . c++-mode)
     ("C++" . c++-mode) ("screen" . shell-script-mode)
     ("shell" . sh-mode) ("bash" . sh-mode) ("ts" . typescript-mode)
     ("js" . javascript-mode) ("json" . json-mode)
     ("rust" . rust-mode) ("sql" . sql-mode) ("python" . python-mode)))
 '(markdown-fontify-code-blocks-natively t)
 '(org-agenda-files '("/home/jonathan/projects/proton/misc/journal.org"))
 '(package-selected-packages
   '(ag breadcrumb claude-code company-phpactor consult-eglot dape
        difftastic edit-indirect embark-consult forge git-link
        gitlab-ci-mode graphviz-dot-mode kotlin-mode lice marginalia
        mermaid-mode mermaid-ts-mode orderless php-cs-fixer php-mode
        plantuml-mode poetry protobuf-mode rainbow-delimiters rustic
        solarized-theme swift-mode treemacs treesit-fold vertico
        web-mode yasnippet-snippets))
 '(plantuml-jar-path "/usr/share/java/plantuml.jar")
 '(rustic-lsp-client 'eglot)
 '(safe-local-variable-values
   '((lsp-rust-analyzer-server-settings
      (check
       (overrideCommand
        . ["cargo" "check" "+esp" "--message-format=json"
           "--all-targets"])))
     (php-project-root . git)
     (php-project-root . default-directory)))
 '(savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
 '(savehist-file "~/.emacs.d/savehist")
 '(savehist-save-minibuffer-history 1)
 '(split-height-threshold 150)
 '(tooltip-use-echo-area t)
 '(typescript-indent-level 2)
 '(typescript-ts-mode-indent-offset 4)
 '(warning-suppress-types '((lsp-mode) (treesit)))
 '(xref-search-program 'ripgrep))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Tree-sitter support
(use-package treesit
  :when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
  :custom
  (major-mode-remap-alist
   '((python-mode     . python-ts-mode)
     (c-mode          . c-ts-mode)
     (c++-mode        . c++-ts-mode)
     (css-mode        . css-ts-mode)
     (javascript-mode . js-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (js-json-mode    . json-ts-mode)
     (csharp-mode     . csharp-ts-mode)))
  :config
  (add-to-list 'auto-mode-alist
               '("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.exs?\\'" . elixir-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.heex\\'" . heex-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\.mod\\'" . go-mod-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode)))

(use-package treesit-fold
  :ensure t
  :init
  (global-treesit-fold-mode )
  :bind
  ("C-c t o" . treesit-fold-open)
  ("C-c t f" . treesit-fold-close)
  ("C-c t F" . treesit-fold-close-all)
  ("C-c t O" . treesit-fold-open-all)
  )

(require 'org)
(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (lisp . t)
         (shell . t)
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

(use-package flyspell
  :custom-face
  (flyspell-incorrect ((t (:underline (:color "violet" :style wave :position nil)))))
  :hook (text-mode-hook . flyspell-mode)
  (prog-mode-hook . flyspell-prog-mode)
  )

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :custom
  (markdown-fontify-code-block-natively t)
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package ace-window
  :ensure t)
(global-set-key (kbd "C-c o") 'ace-window)

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  ;;:init

  ;; Optionally replace the key help with a completing-read interface
  ;; (setq prefix-help-command #'embark-prefix-help-command
  ;;       embark-indicators
  ;;       '(embark-minimal-indicator  ; default is embark-mixed-indicator
  ;;         embark-highlight-indicator
  ;;         embark-isearch-highlight-indicator)
  ;;       )

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;;(add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;;(setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; https://github.com/minad/consult/wiki#minads-orderless-configuration
(use-package vertico
  :ensure t
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  (vertico-multiform-mode)
  :init
  (keymap-set vertico-map "TAB" #'minibuffer-complete)
  (vertico-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package orderless
  :ensure t
  :demand t
  :config

  (defun +orderless--consult-suffix ()
    "Regexp which matches the end of string with Consult tofu support."
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))

  ;; Recognizes the following patterns:
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-consult-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--consult-suffix))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--consult-suffix))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  ;; Certain dynamic completion tables (completion-table-dynamic) do not work
  ;; properly with orderless. One can add basic as a fallback.  Basic will only
  ;; be used when orderless fails, which happens only for these special
  ;; tables. Also note that you may want to configure special styles for special
  ;; completion categories, e.g., partial-completion for files.
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;;; Enable partial-completion for files.
        ;;; Either give orderless precedence or partial-completion.
        ;;; Note that completion-category-overrides is not really an override,
        ;;; but rather prepended to the default completion-styles.
        ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
        completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
                                        ;; enable initialism by default for symbols
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers (list #'+orderless-consult-dispatch
                                          #'orderless-affix-dispatch)))

(use-package mermaid-ts-mode
  :ensure t)

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
(use-package lice
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status))
  )

(use-package difftastic-bindings
  :ensure difftastic ;; or nil if you prefer manual installation
  :config (difftastic-bindings-mode))

(use-package forge
  :ensure t
  :after magit)

(use-package gitlab-ci-mode
  :ensure t)

(use-package treemacs
  :ensure t)

(use-package go-ts-mode
  :custom (go-ts-mode-indent-offset 4)
  )

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

(use-package rustic
  :ensure t
  :after (rust-mode)
  :hook
  (rust-mode-hook . yas-minor-mode)
  )

(use-package pyvenv :ensure t)

(use-package plantuml-mode
  :ensure t
  :custom (plantuml-executable-path "/usr/bin/plantuml")
  (plantuml-default-exec-mode 'executable)
  :config (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

;; (setq help-at-pt-display-when-idle t)
;; (help-at-pt-set-timer)

(use-package graphviz-dot-mode
  :ensure t)

(use-package git-link
  :ensure t
  )

(use-package typescript-ts-mode
  :mode ("\\.ts$" "\\.tsx$"))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/custom/"))

(use-package eglot
  :ensure t
  :after yasnippet-snippets
  :custom
  (eglot-autoshutdown t)
  (eglot-display-mapping-mode t)
  (company-show-numbers t)
  (eglot-enable-eldoc-preview t)
  :bind (:map eglot-mode-map
	      ("C-c l a" . eglot-code-actions)
	      ("C-c l r" . eglot-rename)
	      ("C-c l h" . eldoc)
	      ("C-c l f" . eglot-format)
	      ("C-c l i" . eglot-find-implementation)
	      ("C-c l F" . eglot-format-buffer))
  :hook
  (python-ts-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure)
  (rust-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (c-ts-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  (elixir-ts-mode . eglot-ensure)
  (heex-ts-mod . eglot-ensure)
  (eglot-mode . (lambda () (deactivate-mark)))
  :config
  ;; Force eglot to use pylsp for both standard and tree-sitter python modes
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("poetry" "run" "pylsp")))
  (add-to-list 'eglot-server-programs
               '((elixir-ts-mode heex-ts-mode) . ("expert" "--stdio")))
  )

(use-package consult-eglot
  :ensure t
  :after consult
  )

(use-package breadcrumb
  :ensure t
  :after eglot
  :init
  (breadcrumb-mode 1)
  :custom
  (breadcrumb-project-max-length 16)
)

(use-package dape
  :ensure t
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  :hook
  ;; Save breakpoints on quit
  (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  (after-init . dape-breakpoint-load)

  :custom
  ;; Turn on global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode +1)

  ;; Info buffers to the right
  (dape-buffer-window-arrangement 'right)
  ;; Info buffers like gud (gdb-mi)
  (dape-buffer-window-arrangement 'gud)
  (dape-info-hide-mode-line nil)

  ;; Projectile users
  ;; (dape-cwd-function #'projectile-project-root)

  :config
  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook #'kill-buffer)
  )

;; For a more ergonomic Emacs and `dape' experience
(use-package repeat
  :custom
  (repeat-mode +1))

;; Left and right side windows occupy full frame height
(use-package emacs
  :custom
  (window-sides-vertical t))

(defun copy-file-link-to-clipboard ()
  "Copy current line in file to clipboard as '<filename>:<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))
(global-set-key (kbd "C-c j l") 'copy-file-link-to-clipboard)

(if (file-exists-p "~/.proton") (progn
                                  (add-to-list 'load-path (expand-file-name "~/.emacs.d/proton/"))
                                        (require 'proton)))

(if (file-exists-p "~/.emacs.d/init-ai") (require 'ai))

(provide 'init)
;;; init.el ends here
(put 'downcase-region 'disabled nil)

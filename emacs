;; .emacs

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)
(setq inhibit-startup-screen t)

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;; always end a file with a newline
;(setq require-final-newline 'query)

;; ========== Place Backup Files in Specific Directory ==========

;; Enable backup files.
(setq make-backup-files t)

;; Enable versioning with default values (keep five last versions, I think!)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)


;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;; Keep case on expansion
(setq dabbrev-case-replace nil)

(define-key global-map [f9] 'bookmark-jump)
(define-key global-map [f10] 'bookmark-set)

 (add-to-list 'auto-mode-alist '("\\.module$" . drupal-mode))
 (add-to-list 'auto-mode-alist '("\\.inc$" . drupal-mode))
 (add-to-list 'auto-mode-alist '("\\.install$" . drupal-mode))

(defun wicked/php-mode-init ()
  "Set some buffer-local variables."
  (setq case-fold-search t)
  (setq indent-tabs-mode nil)
  (setq fill-column 78)
  (setq c-basic-offset 4)
  (c-set-offset 'arglist-cont 0)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'case-label 4)
  (c-set-offset 'arglist-close 0))
(add-hook 'php-mode-hook 'wicked/php-mode-init)


; Open root files with sudo 
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
 
(defun sudo-edit-current-file ()
  (interactive)
  (let ((pos (point)))
  (find-alternate-file (concat "/sudo:root@localhost:" (buffer-file-name (current-buffer))))
  (goto-char pos)))
(global-set-key (kbd "C-c C-r") 'sudo-edit-current-file)

(add-to-list 'load-path "~/.emacs.d")

; My PHP setup
;; (require 'drupal-php)
;; (drupal-php)

;; Need to find auto solution based on curr path
;(defvar drupal-project-path "/var/shared/sites/coursecal/site")

;; Tags
;(require 'etags)
;(setq tags-file-name (expand-file-name "tags" drupal-project-path))

(require 'etags-table)
(setq tag-table-alist 
      '(("~/.emacs.d/" . "~/.emacs.d/TAGS")
        ("~/projects/source/" . "~/.TAGS/projects.tags")))
(setq etags-table-alist tag-table-alist)
(setq etags-table-search-up-depth 10)

(defun compile-tags ()
  "compile etags for the current project"
  (interactive)
  (compile "find . -name '*.module' -o -name '*.inc' | etags -a -l php -"))

;; Encrypt / Decrypt .gpg files
(require 'epa-file)
(epa-file-enable)
;; Do not use gpg agent when runing in terminal
    (defadvice epg--start (around advice-epg-disable-agent activate)
      (let ((agent (getenv "GPG_AGENT_INFO")))
        (when (not (display-graphic-p))
          (setenv "GPG_AGENT_INFO" nil))
        ad-do-it
        (when (not (display-graphic-p))
          (setenv "GPG_AGENT_INFO" agent))))

;; (load-library  "~/.emacs.d/vendor/php-htm-mode/multi-mode.el")
;; (load-library  "~/.emacs.d/vendor/php-htm-mode/php-htm-mode.el")

(autoload 'php-mode "php-mode" "PHP mode." t)

(add-hook 'php-mode-hook (lambda ()
    (defun ywb-php-lineup-arglist-intro (langelem)
      (save-excursion
        (goto-char (cdr langelem))
        (vector (+ (current-column) c-basic-offset))))
    (defun ywb-php-lineup-arglist-close (langelem)
      (save-excursion
        (goto-char (cdr langelem))
        (vector (current-column))))
    (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
    (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close))
)

;; Do not indent substatement open, that is after statement ({[...
(setq substatement-open 0)

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

; force emacs to use spaces
(setq-default indent-tabs-mode nil)

(menu-bar-mode -1)

;; (defun tavish-alert ()
;;   (when (and (stringp buffer-file-name)
;;              (string-match ".*\\(bashrc\\|emacs\\)" buffer-file-name))
;;   (start-process "running-alert" nil "mplayer" "/home/jonathan/siren.mp3")
;;   (start-process "lock-screen" nil "xscreensaver-command" "-lock")
;;   (start-process "take-pics" nil "/home/jonathan/script.sh")
;;   ))


;; (add-hook 'find-file-hook 'tavish-alert)

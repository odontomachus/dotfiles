;;; ai.el --- Configure ai tools
;;; Commentary:
;;; Code:

(use-package gptel
  :ensure t)

(gptel-make-gemini "Gemini pro"
   :key (secrets-get-secret "kdewallet" "api-keys/gemini-api-key")
   :stream t)

;; (setq
;;  gptel-model 'gemini-2.5-flash
;;  gptel-backend (gptel-make-gemini "Gemini flash"
;;                  :key (secrets-get-secret "kdewallet" "api-keys/gemini-api-key")
;;                  :stream t))

(setq
 gptel-model 'mistral-small
 gptel-backend
 (gptel-make-openai "Mistral-small"  ;Any name you want
   :host "api.mistral.ai"
   :endpoint "/v1/chat/completions"
   :protocol "https"
   :key  (secrets-get-secret "kdewallet" "work-api-keys/mistral")
   :models '("mistral-small")))

(gptel-make-openai "Mistral-medium"  ;Any name you want
   :host "api.mistral.ai"
   :endpoint "/v1/chat/completions"
   :protocol "https"
   :key  (secrets-get-secret "kdewallet" "work-api-keys/mistral")
   :models '("magistral-medium-latest"))

(use-package vterm :ensure t)

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

(provide 'ai)
;;; ai.el ends here
